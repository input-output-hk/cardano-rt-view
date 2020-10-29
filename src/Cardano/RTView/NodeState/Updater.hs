{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if defined(mingw32_HOST_OS)
#define WINDOWS
#endif
#if defined(linux_HOST_OS)
#define LINUX
#endif
#if defined(darwin_HOST_OS)
#define DARWIN
#endif

module Cardano.RTView.NodeState.Updater
    ( launchNodeStateUpdater
    ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar.Strict (MVar, modifyMVar_)
import           Control.Monad (forever, forM_)
import qualified Data.Aeson as A
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, diffUTCTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Cardano.BM.Backend.Switchboard (Switchboard, readLogBuffer)
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Counter (Platform (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..),
                                          MonitorAction (..), utc2ns)
import           Cardano.BM.Trace (Trace)

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.ErrorBuffer (ErrorBuffer, readErrorBuffer)
import           Cardano.RTView.NodeState.Parsers (extractPeersInfo)
import           Cardano.RTView.NodeState.Types

-- | This function is running in a separate thread.
--   It takes |LogObject|s with nodes' metrics from |LogBuffer|,
--   extracts these metrics and updates corresponding values
--   in the |NodesState|.
launchNodeStateUpdater
  :: Trace IO Text
  -> RTViewParams
  -> Switchboard Text
  -> ErrorBuffer Text
  -> MVar NodesState
  -> IO ()
launchNodeStateUpdater _tr params switchBoard errBuff nsMVar = forever $ do
  -- logDebug tr "Try to update nodes' state..."
  -- Take current |LogObject|s from the |ErrorBuffer|.
  currentErrLogObjects <- readErrorBuffer errBuff
  forM_ currentErrLogObjects $ \(loggerName, errLogObject) ->
    updateNodesStateErrors nsMVar loggerName errLogObject
  -- Take current |LogObject|s from the |LogBuffer|.
  currentLogObjects <- readLogBuffer switchBoard
  forM_ currentLogObjects $ \(loggerName, logObject) ->
    updateNodesState params nsMVar loggerName logObject
  -- Check for updates in the |LogBuffer| every second.
  threadDelay 1000000

-- | Update NodeState for particular node based on loggerName.
--   Please note that this function updates only Error-messages (if errors occurred).
updateNodesStateErrors
  :: MVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesStateErrors nsMVar loggerName (LogObject _ aMeta aContent) = do
  -- Check the name of the node this logObject came from.
  -- It is assumed that configuration contains correct names of remote nodes and
  -- loggers for them, for example:
  --   1. "a" - name of remote node in getAcceptAt.
  --   2. "cardano-rt-view.acceptor.a" - name of the logger which receives
  --        |LogObject|s from that node.
  -- So currently logger name for metrics has the following format:
  -- #buffered.cardano-rt-view.acceptor.a.NAME_OF_METRICS_GROUP.NAME_OF_METRIC,
  -- where "a" is the node name (from TraceAcceptor).
  let loggerNameParts = filter (not . T.null) $ T.splitOn "." loggerName
      nameOfNode = loggerNameParts !! 3

  modifyMVar_ nsMVar $ \currentNodesState -> do
    let nodesStateWith :: NodeState -> IO NodesState
        nodesStateWith newState = return $ Map.adjust (const newState) nameOfNode currentNodesState

    case currentNodesState !? nameOfNode of
      Just ns -> nodesStateWith $ updateNodeErrors ns aMeta aContent
      Nothing -> return currentNodesState

updateNodeErrors :: Show a => NodeState -> LOMeta -> LOContent a -> NodeState
updateNodeErrors ns (LOMeta timeStamp _ _ sev _) aContent = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = newErrors
    , errorsChanged = True -- Every error message changes current list of errors by definition.
    }
  newErrors = NodeError timeStamp sev errorMessage : currentErrors
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  errorMessage =
    case aContent of
      LogMessage msg -> show msg
      LogError eMsg -> T.unpack eMsg
      LogStructured obj -> show obj
      LogStructuredText _ txt -> T.unpack txt
      MonitoringEffect (MonitorAlert msg) -> "Monitor alert: " <> T.unpack msg
      MonitoringEffect _ -> ""
      _ -> "UNPARSED_ERROR_MESSAGE"

-- | Update NodeState for particular node based on loggerName.
updateNodesState
  :: RTViewParams
  -> MVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesState params nsMVar loggerName (LogObject aName aMeta aContent) = do
  -- Check the name of the node this logObject came from.
  -- It is assumed that configuration contains correct names of remote nodes and
  -- loggers for them, for example:
  --   1. "a" - name of remote node in getAcceptAt.
  --   2. "cardano-rt-view.acceptor.a" - name of the logger which receives
  --        |LogObject|s from that node.
  -- So currently logger name for metrics has the following format:
  -- #buffered.cardano-rt-view.acceptor.a.NAME_OF_METRICS_GROUP.NAME_OF_METRIC,
  -- where "a" is the node name (from TraceAcceptor).
  let loggerNameParts = filter (not . T.null) $ T.splitOn "." loggerName
      nameOfNode = loggerNameParts !! 3

  now <- getMonotonicTimeNSec

  modifyMVar_ nsMVar $ \currentNodesState -> do
    let nodesStateWith :: NodeState -> IO NodesState
        nodesStateWith newState = return $ Map.adjust (const newState) nameOfNode currentNodesState

    case currentNodesState !? nameOfNode of
      Just ns ->
        if | "cardano.node.metrics.peersFromNodeKernel" `T.isInfixOf` aName ->
            case aContent of
              LogStructured newPeersInfo ->
                nodesStateWith $ updatePeersInfo ns newPeersInfo
              _ -> return currentNodesState
           | "cardano.node.metrics" `T.isInfixOf` aName ->
            case aContent of
              LogValue "upTime" (Nanoseconds upTimeInNs) ->
                nodesStateWith $ updateNodeUpTime ns upTimeInNs now
              LogValue "txsInMempool" (PureI txsInMempool) ->
                nodesStateWith $ updateMempoolTxs ns txsInMempool
              LogValue "mempoolBytes" (PureI mempoolBytes') ->
                nodesStateWith $ updateMempoolBytes ns mempoolBytes'
              LogValue "txsProcessedNum" (PureI processedTxsNum) ->
                nodesStateWith $ updateTxsProcessed ns processedTxsNum
              LogValue "blocksForgedNum" (PureI forgedBlocksNum) ->
                nodesStateWith $ updateBlocksForged ns forgedBlocksNum now
              LogValue "nodeCannotForge" (PureI cannotForge) ->
                nodesStateWith $ updateNodeCannotForge ns cannotForge
              LogValue "nodeIsLeaderNum" (PureI leaderNum) ->
                nodesStateWith $ updateNodeIsLeader ns leaderNum now
              LogValue "slotsMissedNum" (PureI missedSlotsNum) ->
                nodesStateWith $ updateSlotsMissed ns missedSlotsNum now
              _ -> return currentNodesState
           | "cardano.node-metrics" `T.isInfixOf` aName ->
            case aContent of
#ifdef WINDOWS
              LogValue "Stat.CPUTime" (Microseconds microsecs) ->
                nodesStateWith $ updateCPUSecs ns (microsecs * 1000) aMeta now
#endif
#ifdef DARWIN
              LogValue "Mem.resident_size" (Bytes bytes) ->
                nodesStateWith $ updateMemoryBytes ns bytes now
              LogValue "Sys.CPUTime" (Nanoseconds nanosecs) ->
                nodesStateWith $ updateCPUSecs ns nanosecs aMeta now
              LogValue "Net.ifd_0-ibytes" (Bytes inBytes) ->
                nodesStateWith $ updateNetworkIn ns inBytes aMeta now
              LogValue "Net.ifd_0-obytes" (Bytes outBytes) ->
                nodesStateWith $ updateNetworkOut ns outBytes aMeta now
#endif
#ifdef LINUX
              LogValue "Mem.resident" (PureI pages) ->
                nodesStateWith $ updateMemoryPages ns pages now
              LogValue "IO.rchar" (Bytes bytesWereRead) ->
                nodesStateWith $ updateDiskRead ns bytesWereRead aMeta now
              LogValue "IO.wchar" (Bytes bytesWereWritten) ->
                nodesStateWith $ updateDiskWrite ns bytesWereWritten aMeta now
              LogValue "Stat.cputicks" (PureI ticks) ->
                nodesStateWith $ updateCPUTicks ns ticks aMeta now
              LogValue "Net.IpExt:InOctets" (Bytes inBytes) ->
                nodesStateWith $ updateNetworkIn ns inBytes aMeta now
              LogValue "Net.IpExt:OutOctets" (Bytes outBytes) ->
                nodesStateWith $ updateNetworkOut ns outBytes aMeta now
#endif
              LogValue "Sys.Platform" (PureI pfid) ->
                nodesStateWith $ updateNodePlatform ns (fromIntegral pfid)
              LogValue "RTS.maxUsedMemBytes" (Bytes bytesAllocated) ->
                nodesStateWith $ updateRTSBytesAllocated ns bytesAllocated now
              LogValue "RTS.gcLiveBytes" (Bytes usedMemBytes) ->
                nodesStateWith $ updateRTSBytesUsed ns usedMemBytes now
              LogValue "RTS.gcCpuNs" (Nanoseconds gcCpuNs) ->
                nodesStateWith $ updateGcCpuNs ns gcCpuNs now
              LogValue "RTS.gcElapsedNs" (Nanoseconds gcElapsedNs) ->
                nodesStateWith $ updateGcElapsedNs ns gcElapsedNs now
              LogValue "RTS.gcNum" (PureI gcNum) ->
                nodesStateWith $ updateGcNum ns gcNum now
              LogValue "RTS.gcMajorNum" (PureI gcMajorNum) ->
                nodesStateWith $ updateGcMajorNum ns gcMajorNum now
              _ -> return currentNodesState
           | "cardano.node.Forge.metrics" `T.isInfixOf` aName ->
            case aContent of
              LogValue "operationalCertificateStartKESPeriod" (PureI oCertStartKesPeriod) ->
                nodesStateWith $ updateCertStartKESPeriod ns oCertStartKesPeriod now
              LogValue "operationalCertificateExpiryKESPeriod" (PureI oCertExpiryKesPeriod) ->
                nodesStateWith $ updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now
              LogValue "currentKESPeriod" (PureI currentKesPeriod) ->
                nodesStateWith $ updateCurrentKESPeriod ns currentKesPeriod now
              LogValue "remainingKESPeriods" (PureI kesPeriodsUntilExpiry) ->
                nodesStateWith $ updateRemainingKESPeriods ns params kesPeriodsUntilExpiry now
              _ -> return currentNodesState
           | "cardano.node.release" `T.isInfixOf` aName ->
            case aContent of
              LogMessage protocol ->
                nodesStateWith $ updateNodeProtocol ns protocol
              _ -> return currentNodesState
           | "cardano.node.version" `T.isInfixOf` aName ->
            case aContent of
              LogMessage version ->
                nodesStateWith $ updateNodeVersion ns version
              _ -> return currentNodesState
           | "cardano.node.commit" `T.isInfixOf` aName ->
            case aContent of
              LogMessage commit ->
                nodesStateWith $ updateNodeCommit ns commit
              _ -> return currentNodesState
           | otherwise ->
            case aContent of
              LogValue "density" (PureD density) ->
                nodesStateWith $ updateChainDensity ns density now
              LogValue "blockNum" (PureI blockNum) ->
                nodesStateWith $ updateBlocksNumber ns blockNum now
              LogValue "slotInEpoch" (PureI slotNum) ->
                nodesStateWith $ updateSlotInEpoch ns slotNum now
              LogValue "epoch" (PureI epoch') ->
                nodesStateWith $ updateEpoch ns epoch' now
              _ -> return currentNodesState
      Nothing ->
        -- This is a problem, because it means that configuration is unexpected one:
        -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
        return currentNodesState

  -- Update elements of GUI iff the new corresponding values were received.
  -- probablyUpdatedState <- readMVar nsMVar
  return ()

-- Updaters for particular node state's fields.

updateNodeUpTime :: NodeState -> Word64 -> Word64 -> NodeState
updateNodeUpTime ns upTimeInNs now = ns { nodeMetrics = newNodeMetrics }
 where
  newNodeMetrics = (nodeMetrics ns)
    { upTime = upTimeInNs
    , upTimeLastUpdate = now
    }

updatePeersInfo :: NodeState -> A.Object -> NodeState
updatePeersInfo ns newPeersInfo = ns { peersMetrics = newPeersMetrics }
 where
  newPeersMetrics = currentMetrics
    { peersInfo = newPeersInfo'
    , peersInfoChanged = peersInfo currentMetrics /= newPeersInfo'
    }
  currentMetrics = peersMetrics ns
  newPeersInfo' = extractPeersInfo newPeersInfo

updateNodeProtocol :: NodeState -> Text -> NodeState
updateNodeProtocol ns protocol = ns { nodeMetrics = newNodeMetrics }
 where
  newNodeMetrics = currentMetrics
    { nodeProtocol = protocol
    , nodeProtocolChanged = nodeProtocol currentMetrics /= protocol
    }
  currentMetrics = nodeMetrics ns

updateNodeVersion :: NodeState -> Text -> NodeState
updateNodeVersion ns version = ns { nodeMetrics = newNodeMetrics }
 where
  newNodeMetrics = currentMetrics
    { nodeVersion = version
    , nodeVersionChanged = nodeVersion currentMetrics /= version
    }
  currentMetrics = nodeMetrics ns

updateNodeCommit :: NodeState -> Text -> NodeState
updateNodeCommit ns commit = ns { nodeMetrics = newNodeMetrics }
 where
  newNodeMetrics = currentMetrics
    { nodeCommit = commit
    , nodeCommitChanged = nodeCommit currentMetrics /= commit
    , nodeShortCommit = T.take 7 commit
    }
  currentMetrics = nodeMetrics ns

updateNodePlatform :: NodeState -> Int -> NodeState
updateNodePlatform ns platfId = ns { nodeMetrics = newNodeMetrics }
 where
  newNodeMetrics = currentMetrics
    { nodePlatform = platformName
    , nodePlatformChanged = True -- nodePlatform currentMetrics /= platformName
    }
  currentMetrics = nodeMetrics ns
  platformName = T.pack . show $ (toEnum platfId :: Platform)

#ifdef LINUX
updateMemoryPages :: NodeState -> Integer -> Word64 -> NodeState
updateMemoryPages ns pages now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { memory           = mBytes
      , memoryChanged    = memory currentMetrics /= mBytes
      , memoryMax        = newMax
      , memoryMaxTotal   = newMaxTotal
      , memoryPercent    = mBytes / newMaxTotal * 100.0
      , memoryLastUpdate = now
      }
  currentMetrics   = resourcesMetrics ns
  prevMax     = memoryMax currentMetrics
  newMax      = max prevMax mBytes
  newMaxTotal = max newMax 200.0
  mBytes      = fromIntegral (pages * pageSize) / 1024 / 1024 :: Double
  pageSize    = 4096 :: Integer
#endif

#ifdef DARWIN
updateMemoryBytes :: NodeState -> Word64 -> Word64 -> NodeState
updateMemoryBytes ns bytes now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { memory           = mBytes
      , memoryChanged    = currentMemory /= mBytes
      , memoryMax        = newMax
      , memoryMaxTotal   = newMaxTotal
      , memoryPercent    = mBytes / newMaxTotal * 100.0
      , memoryLastUpdate = now
      }
  currentMetrics   = resourcesMetrics ns
  currentMemory = memory currentMetrics
  prevMax     = memoryMax currentMetrics
  newMax      = max prevMax mBytes
  newMaxTotal = max newMax 200.0
  mBytes      = fromIntegral bytes / 1024 / 1024 :: Double
#endif

#ifdef LINUX
updateDiskRead :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateDiskRead ns bytesWereRead meta now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { diskUsageR           = currentDiskRate
      , diskUsageRChanged    = diskUsageR currentMetrics /= currentDiskRate
      , diskUsageRPercent    = diskUsageRPercent'
      , diskUsageRLast       = bytesWereRead
      , diskUsageRNs         = currentTimeInNs
      , diskUsageRMax        = maxDiskRate
      , diskUsageRMaxTotal   = max maxDiskRate 1.0
      , diskUsageRAdaptTime  = newAdaptTime
      , diskUsageRLastUpdate = now
      }
  currentMetrics         = resourcesMetrics ns
  currentTimeInNs   = utc2ns (tstamp meta)
  timeDiff          = fromIntegral (currentTimeInNs - diskUsageRNs currentMetrics) :: Double
  timeDiffInSecs    = timeDiff / 1000000000
  bytesDiff         = fromIntegral (bytesWereRead - diskUsageRLast currentMetrics) :: Double
  bytesDiffInKB'    = bytesDiff / 1024
  bytesDiffInKB     = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                        then 1.0
                        else bytesDiffInKB'
  currentDiskRate   = bytesDiffInKB / timeDiffInSecs
  lastAdaptTime     = diskUsageRAdaptTime currentMetrics
  timeElapsed       = diffUTCTime (tstamp meta) lastAdaptTime
  ( maxDiskRate
    , newAdaptTime ) =
        if timeElapsed >= adaptPeriod
          then ((diskUsageRMax currentMetrics + currentDiskRate) / 2, tstamp meta)
          else (max currentDiskRate $ diskUsageRMax currentMetrics, lastAdaptTime)
  diskUsageRPercent' = currentDiskRate / (maxDiskRate / 100.0)

updateDiskWrite :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateDiskWrite ns bytesWereWritten meta now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { diskUsageW           = currentDiskRate
      , diskUsageWChanged    = diskUsageW currentMetrics /= currentDiskRate
      , diskUsageWPercent    = diskUsageWPercent'
      , diskUsageWLast       = bytesWereWritten
      , diskUsageWNs         = currentTimeInNs
      , diskUsageWMax        = maxDiskRate
      , diskUsageWMaxTotal   = max maxDiskRate 1.0
      , diskUsageWAdaptTime  = newAdaptTime
      , diskUsageWLastUpdate = now
      }
  currentMetrics         = resourcesMetrics ns
  currentTimeInNs   = utc2ns (tstamp meta)
  timeDiff          = fromIntegral (currentTimeInNs - diskUsageWNs currentMetrics) :: Double
  timeDiffInSecs    = timeDiff / 1000000000
  bytesDiff         = fromIntegral (bytesWereWritten - diskUsageWLast currentMetrics) :: Double
  bytesDiffInKB'    = bytesDiff / 1024
  bytesDiffInKB     = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                        then 1.0
                        else bytesDiffInKB'
  currentDiskRate   = bytesDiffInKB / timeDiffInSecs
  lastAdaptTime     = diskUsageWAdaptTime currentMetrics
  timeElapsed       = diffUTCTime (tstamp meta) lastAdaptTime
  ( maxDiskRate
    , newAdaptTime ) =
        if timeElapsed >= adaptPeriod
          then ((diskUsageWMax currentMetrics + currentDiskRate) / 2, tstamp meta)
          else (max currentDiskRate $ diskUsageWMax currentMetrics, lastAdaptTime)
  diskUsageWPercent' = currentDiskRate / (maxDiskRate / 100.0)

-- | Adaptaion period for disk usage max values.
--   We have to adapt the max value to the new situation periodically,
--   because it might get very high once, and then it will stay there forever.
adaptPeriod :: NominalDiffTime
adaptPeriod = fromInteger $ 60 * 2 -- 2 minutes.

updateCPUTicks :: NodeState -> Integer -> LOMeta -> Word64 -> NodeState
updateCPUTicks ns ticks meta now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { cpuPercent    = newCPUPercent
      , cpuPercentChanged = cpuPercent currentMetrics /= newCPUPercent
      , cpuLast       = ticks
      , cpuNs         = tns
      , cpuLastUpdate = now
      }
  currentMetrics = resourcesMetrics ns
  newCPUPercent = cpuperc * 100.0
  tns       = utc2ns $ tstamp meta
  tdiff     = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000000000 :: Double
  cpuperc   = fromIntegral (ticks - cpuLast currentMetrics) / fromIntegral clktck / tdiff
  clktck    = 100 :: Integer
#endif

#if defined(DARWIN) || defined(LINUX)
updateNetworkIn :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateNetworkIn ns inBytes meta now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { networkUsageIn           = currentNetRate
      , networkUsageInChanged    = networkUsageIn currentMetrics /= currentNetRate
      , networkUsageInPercent    = currentNetRate / (maxNetRate / 100.0)
      , networkUsageInLast       = inBytes
      , networkUsageInNs         = currentTimeInNs
      , networkUsageInMax        = maxNetRate
      , networkUsageInMaxTotal   = max maxNetRate 1.0
      , networkUsageInLastUpdate = now
      }
  currentMetrics       = resourcesMetrics ns
  currentTimeInNs = utc2ns (tstamp meta)
  timeDiff        = fromIntegral (currentTimeInNs - networkUsageInNs currentMetrics) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (inBytes - networkUsageInLast currentMetrics) :: Double
  bytesDiffInKB   = bytesDiff / 1024
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  maxNetRate      = max currentNetRate $ networkUsageInMax currentMetrics

updateNetworkOut :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateNetworkOut ns outBytes meta now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { networkUsageOut           = currentNetRate
      , networkUsageOutChanged    = networkUsageOut currentMetrics /= currentNetRate
      , networkUsageOutPercent    = currentNetRate / (maxNetRate / 100.0)
      , networkUsageOutLast       = outBytes
      , networkUsageOutNs         = currentTimeInNs
      , networkUsageOutMax        = maxNetRate
      , networkUsageOutMaxTotal   = max maxNetRate 1.0
      , networkUsageOutLastUpdate = now
      }
  currentMetrics       = resourcesMetrics ns
  currentTimeInNs = utc2ns (tstamp meta)
  timeDiff        = fromIntegral (currentTimeInNs - networkUsageOutNs currentMetrics) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (outBytes - networkUsageOutLast currentMetrics) :: Double
  bytesDiffInKB   = bytesDiff / 1024
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  maxNetRate      = max currentNetRate $ networkUsageOutMax currentMetrics
#endif

#if defined(DARWIN) || defined(WINDOWS)
updateCPUSecs :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateCPUSecs ns nanosecs meta now = ns { resourcesMetrics = newMetrics }
 where
  newMetrics =
    currentMetrics
      { cpuPercent    = newCPUPercent
      , cpuPercentChanged = cpuPercent currentMetrics /= newCPUPercent
      , cpuLast       = fromIntegral nanosecs
      , cpuNs         = tns
      , cpuLastUpdate = now
      }
  currentMetrics = resourcesMetrics ns
  newCPUPercent = if cpuperc < 0
                    then 0
                    else if cpuperc > 20.0
                           then nmCPUPercent currentMetrics
                           else cpuperc * 100.0
  tns       = utc2ns $ tstamp meta
  tdiff     = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000000000 :: Double
  deltacpu  = fromIntegral nanosecs - cpuLast currentMetrics
  cpuperc   = fromIntegral deltacpu / 100000000 / tdiff
#endif

updateMempoolTxs :: NodeState -> Integer -> NodeState
updateMempoolTxs ns txsInMempool = ns { mempoolMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { mempoolTxsNumber = fromIntegral txsInMempool
    , mempoolTxsNumberChanged = currentMempoolTxsNumber /= fromIntegral txsInMempool
    , mempoolTxsPercent =   fromIntegral txsInMempool
                          / fromIntegral maxTxs
                          * 100.0
    , mempoolMaxTxs = maxTxs
    }
  currentMetrics = mempoolMetrics ns
  currentMempoolTxsNumber = mempoolTxsNumber currentMetrics
  maxTxs = max txsInMempool (mempoolMaxTxs currentMetrics)

updateMempoolBytes :: NodeState -> Integer -> NodeState
updateMempoolBytes ns newMempoolBytes = ns { mempoolMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { mempoolBytes = fromIntegral newMempoolBytes
    , mempoolBytesChanged = currentMempoolBytes /= fromIntegral newMempoolBytes
    , mempoolBytesPercent =   fromIntegral newMempoolBytes
                            / fromIntegral maxBytes
                            * 100.0
    , mempoolMaxBytes = maxBytes
    }
  currentMetrics = mempoolMetrics ns
  currentMempoolBytes = mempoolBytes currentMetrics
  maxBytes = max newMempoolBytes (mempoolMaxBytes currentMetrics)

updateTxsProcessed :: NodeState -> Integer -> NodeState
updateTxsProcessed ns txsProcNum = ns { mempoolMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { txsProcessed = txsProcNum
    , txsProcessedChanged = txsProcessed currentMetrics /= txsProcNum
    }
  currentMetrics = mempoolMetrics ns

updateBlocksForged :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksForged ns blocksForged now = ns { forgeMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { blocksForgedNumber = blocksForged
    , blocksForgedNumberChanged = blocksForgedNumber currentMetrics /= blocksForged
    , blocksForgedNumberLastUpdate = now
    }
  currentMetrics = forgeMetrics ns

updateNodeCannotForge :: NodeState -> Integer -> NodeState
updateNodeCannotForge ns cannotForge = ns { forgeMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { nodeCannotForge = cannotForge
    , nodeCannotForgeChanged = nodeCannotForge currentMetrics /= cannotForge
    }
  currentMetrics = forgeMetrics ns

updateNodeIsLeader :: NodeState -> Integer -> Word64 -> NodeState
updateNodeIsLeader ns nodeIsLeader now = ns { forgeMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { nodeIsLeaderNum = nodeIsLeader
    , nodeIsLeaderNumChanged = nodeIsLeaderNum currentMetrics /= nodeIsLeader
    , nodeIsLeaderNumLastUpdate = now
    }
  currentMetrics = forgeMetrics ns

updateSlotsMissed :: NodeState -> Integer -> Word64 -> NodeState
updateSlotsMissed ns slotsMissed now = ns { forgeMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { slotsMissedNumber = slotsMissed
    , slotsMissedNumberChanged = slotsMissedNumber currentMetrics /= slotsMissed
    , slotsMissedNumberLastUpdate = now
    }
  currentMetrics = forgeMetrics ns

updateRTSBytesAllocated :: NodeState -> Word64 -> Word64 -> NodeState
updateRTSBytesAllocated ns bytesAllocated now = ns { rtsMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { rtsMemoryAllocated  = newRTSMemoryAllocated
    , rtsMemoryAllocatedChanged = rtsMemoryAllocated currentMetrics /= newRTSMemoryAllocated
    , rtsMemoryLastUpdate = now
    }
  currentMetrics = rtsMetrics ns
  newRTSMemoryAllocated = fromIntegral bytesAllocated / 1024 / 1024 :: Double

updateRTSBytesUsed :: NodeState -> Word64 -> Word64 -> NodeState
updateRTSBytesUsed ns usedMemBytes now = ns { rtsMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { rtsMemoryUsed = mBytes
    , rtsMemoryUsedChanged = rtsMemoryUsed currentMetrics /= mBytes
    , rtsMemoryUsedPercent =   mBytes
                             / rtsMemoryAllocated currentMetrics
                             * 100.0
    , rtsMemoryLastUpdate = now
    }
  currentMetrics = rtsMetrics ns
  mBytes    = fromIntegral usedMemBytes / 1024 / 1024 :: Double

updateGcCpuNs :: NodeState -> Word64 -> Word64 -> NodeState
updateGcCpuNs ns gcCpuNs now = ns { rtsMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { rtsGcCpu = newGcCpu
    , rtsGcCpuChanged = rtsGcCpu currentMetrics /= newGcCpu
    , rtsGcCpuLastUpdate = now
    }
  currentMetrics = rtsMetrics ns
  newGcCpu = fromIntegral gcCpuNs / 1000000000 :: Double

updateGcElapsedNs :: NodeState -> Word64 -> Word64 -> NodeState
updateGcElapsedNs ns gcElapsedNs now = ns { rtsMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { rtsGcElapsed = newGcElapsed
    , rtsGcElapsedChanged = rtsGcElapsed currentMetrics /= newGcElapsed
    , rtsGcElapsedLastUpdate = now
    }
  currentMetrics = rtsMetrics ns
  newGcElapsed = fromIntegral gcElapsedNs / 1000000000 :: Double

updateGcNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcNum ns gcNum now = ns { rtsMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { rtsGcNum = gcNum
    , rtsGcNumChanged = rtsGcNum currentMetrics /= gcNum
    , rtsGcNumLastUpdate = now
    }
  currentMetrics = rtsMetrics ns

updateGcMajorNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcMajorNum ns gcMajorNum now = ns { rtsMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { rtsGcMajorNum = gcMajorNum
    , rtsGcMajorNumChanged = rtsGcMajorNum currentMetrics /= gcMajorNum
    , rtsGcMajorNumLastUpdate = now
    }
  currentMetrics = rtsMetrics ns

updateCertStartKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertStartKESPeriod ns oCertStartKesPeriod now = ns { kesMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { opCertStartKESPeriod = oCertStartKesPeriod
    , opCertStartKESPeriodChanged = opCertStartKESPeriod currentMetrics /= oCertStartKesPeriod
    , opCertStartKESPeriodLastUpdate = now
    }
  currentMetrics = kesMetrics ns

updateCertExpiryKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now = ns { kesMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { opCertExpiryKESPeriod = oCertExpiryKesPeriod
    , opCertExpiryKESPeriodChanged = opCertExpiryKESPeriod currentMetrics /= oCertExpiryKesPeriod
    , opCertExpiryKESPeriodLastUpdate = now
    }
  currentMetrics = kesMetrics ns

updateCurrentKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCurrentKESPeriod ns currentKesPeriod now = ns { kesMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { currentKESPeriod = currentKesPeriod
    , currentKESPeriodChanged = currentKESPeriod currentMetrics /= currentKesPeriod
    , currentKESPeriodLastUpdate = now
    }
  currentMetrics = kesMetrics ns

updateRemainingKESPeriods :: NodeState -> RTViewParams -> Integer -> Word64 -> NodeState
updateRemainingKESPeriods ns params kesPeriodsUntilExpiry now = ns { kesMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { remainingKESPeriods = kesPeriodsUntilExpiry
    , remainingKESPeriodsChanged = remainingKESPeriods currentMetrics /= kesPeriodsUntilExpiry
    , remainingKESPeriodsInDays = floor remainingInDays
    , remainingKESPeriodsLastUpdate = now
    }
  currentMetrics = kesMetrics ns
  remainingInSeconds =   fromIntegral kesPeriodsUntilExpiry
                       * rtvSlotLength params
                       * rtvSlotsPerKESPeriod params
  remainingInDays :: Double
  remainingInDays = fromIntegral remainingInSeconds / 3600 / 24

updateChainDensity :: NodeState -> Double -> Word64 -> NodeState
updateChainDensity ns density now = ns { blockchainMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { chainDensity = newDencity
    , chainDensityChanged = chainDensity currentMetrics /= newDencity
    , chainDensityLastUpdate = now
    }
  currentMetrics = blockchainMetrics ns
  newDencity = 0.05 + density * 100.0

updateBlocksNumber :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksNumber ns blockNum now = ns { blockchainMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { blocksNumber = blockNum
    , blocksNumberChanged = blocksNumber currentMetrics /= blockNum
    , blocksNumberLastUpdate = now
    }
  currentMetrics = blockchainMetrics ns

updateSlotInEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateSlotInEpoch ns slotNum now = ns { blockchainMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { slot = slotNum
    , slotChanged = slot currentMetrics /= slotNum
    , slotLastUpdate = now
    }
  currentMetrics = blockchainMetrics ns

updateEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateEpoch ns newEpoch now = ns { blockchainMetrics = newMetrics }
 where
  newMetrics = currentMetrics
    { epoch = newEpoch
    , epochChanged = epoch currentMetrics /= newEpoch
    , epochLastUpdate = now
    }
  currentMetrics = blockchainMetrics ns
