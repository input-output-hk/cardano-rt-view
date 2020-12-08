{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

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

import           Control.Concurrent.STM.TVar (TVar, modifyTVar')
import           Control.Monad (forever, forM_)
import           Control.Monad.STM (atomically)
import qualified Data.Aeson as A
import           Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.Time.Extra (sleep)

import           Cardano.BM.Backend.Switchboard (Switchboard, readLogBuffer)
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Counter (Platform (..))
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..),
                                          MonitorAction (..), utc2ns)
import           Cardano.BM.Trace (Trace, logDebug)

import           Cardano.RTView.ErrorBuffer (ErrorBuffer, readErrorBuffer)
import           Cardano.RTView.NodeState.Parsers (extractPeersInfo)
import           Cardano.RTView.NodeState.Types

-- | This function is running in a separate thread.
--   It takes |LogObject|s with nodes' metrics from |LogBuffer|,
--   extracts these metrics and updates corresponding values
--   in the |NodesState|.
launchNodeStateUpdater
  :: Trace IO Text
  -> Switchboard Text
  -> ErrorBuffer Text
  -> TVar NodesState
  -> IO ()
launchNodeStateUpdater tr switchBoard errBuff nsTVar = forever $ do
  -- Take current |LogObject|s from the |ErrorBuffer|.
  currentErrLogObjects <- readErrorBuffer errBuff
  forM_ currentErrLogObjects $ \(loggerName, errLogObject) ->
    updateNodesStateErrors tr nsTVar loggerName errLogObject
  -- Take current |LogObject|s from the |LogBuffer|.
  currentLogObjects <- readLogBuffer switchBoard
  forM_ currentLogObjects $ \(loggerName, logObject) ->
    updateNodesState tr nsTVar loggerName logObject
  -- Check for updates in the |LogBuffer| every second.
  sleep 1.0

-- | Update NodeState for particular node based on loggerName.
--   Please note that this function updates only Error-messages (if errors occurred).
updateNodesStateErrors
  :: Trace IO Text
  -> TVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesStateErrors tr nsTVar loggerName (LogObject aName aMeta aContent) = do
  logDebug tr $ "New error, name: " <> aName
              <> ", meta: " <> T.pack (show aMeta)
              <> ", content: " <> T.pack (show aContent)
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

  atomically $ modifyTVar' nsTVar $ \currentNodesState ->
    let nsWith :: NodeState -> NodesState
        nsWith newState = HM.adjust (const newState) nameOfNode currentNodesState
    in
    case currentNodesState !? nameOfNode of
      Just ns -> nsWith $ updateNodeErrors ns aMeta aContent
      Nothing -> currentNodesState

updateNodeErrors :: Show a => NodeState -> LOMeta -> LOContent a -> NodeState
updateNodeErrors ns (LOMeta timeStamp _ _ sev _) aContent = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = newErrors
    , errorsChanged = True -- Every error message changes current list of errors by definition.
    , errorsRebuild = False -- By default we shouldn't rebuild all the list.
    }
  newErrors = currentErrors ++ [NodeError timeStamp sev errorMessage visible]
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
  visible = True

-- | Update NodeState for particular node based on loggerName.
updateNodesState
  :: Trace IO Text
  -> TVar NodesState
  -> Text
  -> LogObject Text
  -> IO ()
updateNodesState tr nsTVar loggerName (LogObject aName aMeta aContent) = do
  logDebug tr $ "New logObject, name: " <> aName
                <> ", meta: " <> T.pack (show aMeta)
                <> ", content: " <> T.pack (show aContent)
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

  atomically $ modifyTVar' nsTVar $ \currentNodesState ->
    let nsWith :: NodeState -> NodesState
        nsWith newState = HM.adjust (const newState) nameOfNode currentNodesState
        itIs name' = name' `T.isInfixOf` aName
        textValue updater =
          case aContent of
            LogMessage txtValue -> nsWith $ updater txtValue
            _ -> currentNodesState
    in
    case currentNodesState !? nameOfNode of
      Just ns ->
        if | itIs "cardano.node.metrics.peersFromNodeKernel" ->
            case aContent of
              LogStructured newPeersInfo ->
                nsWith $ updatePeersInfo ns newPeersInfo now
              _ -> currentNodesState
           | itIs "cardano.node.metrics" ->
            case aContent of
              LogValue "txsInMempool" (PureI txsInMempool) ->
                nsWith $ updateMempoolTxs ns txsInMempool now
              LogValue "mempoolBytes" (PureI mempoolBytes') ->
                nsWith $ updateMempoolBytes ns mempoolBytes' now
              LogValue "txsProcessedNum" (PureI processedTxsNum) ->
                nsWith $ updateTxsProcessed ns processedTxsNum now
              LogValue "blocksForgedNum" (PureI forgedBlocksNum) ->
                nsWith $ updateBlocksForged ns forgedBlocksNum now
              LogValue "nodeCannotForge" (PureI cannotForge) ->
                nsWith $ updateNodeCannotForge ns cannotForge now
              LogValue "nodeIsLeaderNum" (PureI leaderNum) ->
                nsWith $ updateNodeIsLeader ns leaderNum now
              LogValue "slotsMissedNum" (PureI missedSlotsNum) ->
                nsWith $ updateSlotsMissed ns missedSlotsNum now
              _ -> currentNodesState
           | itIs "cardano.node-metrics" ->
            case aContent of
#ifdef WINDOWS
              LogValue "Stat.CPUTime" (Microseconds microsecs) ->
                nsWith $ updateCPUSecs ns (microsecs * 1000) aMeta now
#endif
#ifdef DARWIN
              LogValue "Mem.resident_size" (Bytes bytes) ->
                nsWith $ updateMemoryBytes ns bytes now
              LogValue "Sys.CPUTime" (Nanoseconds nanosecs) ->
                nsWith $ updateCPUSecs ns nanosecs aMeta now
              LogValue "Net.ifd_0-ibytes" (Bytes inBytes) ->
                nsWith $ updateNetworkIn ns inBytes aMeta now
              LogValue "Net.ifd_0-obytes" (Bytes outBytes) ->
                nsWith $ updateNetworkOut ns outBytes aMeta now
#endif
#ifdef LINUX
              LogValue "Mem.resident" (PureI pages) ->
                nsWith $ updateMemoryPages ns pages now
              LogValue "IO.rchar" (Bytes bytesWereRead) ->
                nsWith $ updateDiskRead ns bytesWereRead aMeta now
              LogValue "IO.wchar" (Bytes bytesWereWritten) ->
                nsWith $ updateDiskWrite ns bytesWereWritten aMeta now
              LogValue "Stat.cputicks" (PureI ticks) ->
                nsWith $ updateCPUTicks ns ticks aMeta now
              LogValue "Net.IpExt:InOctets" (Bytes inBytes) ->
                nsWith $ updateNetworkIn ns inBytes aMeta now
              LogValue "Net.IpExt:OutOctets" (Bytes outBytes) ->
                nsWith $ updateNetworkOut ns outBytes aMeta now
#endif
              LogValue "Sys.Platform" (PureI pfid) ->
                nsWith $ updateNodePlatform ns (fromIntegral pfid) now
              LogValue "RTS.maxUsedMemBytes" (Bytes bytesAllocated) ->
                nsWith $ updateRTSBytesAllocated ns bytesAllocated now
              LogValue "RTS.gcLiveBytes" (Bytes usedMemBytes) ->
                nsWith $ updateRTSBytesUsed ns usedMemBytes now
              LogValue "RTS.gcCpuNs" (Nanoseconds gcCpuNs) ->
                nsWith $ updateGcCpuNs ns gcCpuNs now
              LogValue "RTS.gcElapsedNs" (Nanoseconds gcElapsedNs) ->
                nsWith $ updateGcElapsedNs ns gcElapsedNs now
              LogValue "RTS.gcNum" (PureI gcNum) ->
                nsWith $ updateGcNum ns gcNum now
              LogValue "RTS.gcMajorNum" (PureI gcMajorNum) ->
                nsWith $ updateGcMajorNum ns gcMajorNum now
              _ -> currentNodesState
           | itIs "cardano.node.Forge.metrics" ->
            case aContent of
              LogValue "operationalCertificateStartKESPeriod" (PureI oCertStartKesPeriod) ->
                nsWith $ updateCertStartKESPeriod ns oCertStartKesPeriod now
              LogValue "operationalCertificateExpiryKESPeriod" (PureI oCertExpiryKesPeriod) ->
                nsWith $ updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now
              LogValue "currentKESPeriod" (PureI currentKesPeriod) ->
                nsWith $ updateCurrentKESPeriod ns currentKesPeriod now
              LogValue "remainingKESPeriods" (PureI kesPeriodsUntilExpiry) ->
                nsWith $ updateRemainingKESPeriods ns kesPeriodsUntilExpiry now
              _ -> currentNodesState
           | itIs "basicInfo.protocol" ->
             textValue $ updateNodeProtocol ns now
           | itIs "basicInfo.version" ->
             textValue $ updateNodeVersion ns now
           | itIs "basicInfo.commit" ->
             textValue $ updateNodeCommit ns now
           | itIs "basicInfo.nodeStartTime" ->
             textValue $ updateNodeStartTime ns now
           | itIs "basicInfo.systemStartTime" ->
             textValue $ updateSystemStartTime ns now
           | otherwise ->
            case aContent of
              LogValue "density" (PureD density) ->
                nsWith $ updateChainDensity ns density now
              LogValue "blockNum" (PureI blockNum) ->
                nsWith $ updateBlocksNumber ns blockNum now
              LogValue "slotInEpoch" (PureI slotNum) ->
                nsWith $ updateSlotInEpoch ns slotNum now
              LogValue "epoch" (PureI epoch') ->
                nsWith $ updateEpoch ns epoch' now
              _ -> currentNodesState
      Nothing ->
        -- This is a problem, because it means that configuration is unexpected one:
        -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
        currentNodesState

-- Updaters for particular node state's fields.

updatePeersInfo :: NodeState -> A.Object -> Word64 -> NodeState
updatePeersInfo ns newPeersInfo now = ns { peersMetrics = newPeersMetrics, metricsLastUpdate = now }
 where
  newPeersMetrics = currentMetrics
    { peersInfo = newPeersInfo'
    , peersInfoChanged = peersInfo currentMetrics /= newPeersInfo'
    }
  currentMetrics = peersMetrics ns
  newPeersInfo' = extractPeersInfo newPeersInfo

updateNodeProtocol :: NodeState -> Word64 -> Text -> NodeState
updateNodeProtocol ns now protocol = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeProtocol = protocol
    , nodeProtocolChanged = nodeProtocol currentMetrics /= protocol
    }
  currentMetrics = nodeMetrics ns

updateNodeVersion :: NodeState -> Word64 -> Text -> NodeState
updateNodeVersion ns now version = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeVersion = version
    , nodeVersionChanged = nodeVersion currentMetrics /= version
    }
  currentMetrics = nodeMetrics ns

updateNodeCommit :: NodeState -> Word64 -> Text -> NodeState
updateNodeCommit ns now commit = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeCommit = commit
    , nodeCommitChanged = nodeCommit currentMetrics /= commit
    , nodeShortCommit = T.take 7 commit
    }
  currentMetrics = nodeMetrics ns

updateNodeStartTime :: NodeState -> Word64 -> Text -> NodeState
updateNodeStartTime ns now startTime = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodeStartTime = startTimeUTC
    , nodeStartTimeChanged = nodeStartTime currentMetrics /= startTimeUTC
    }
  currentMetrics = nodeMetrics ns
  startTimeUTC = read (T.unpack startTime) :: UTCTime

updateSystemStartTime :: NodeState -> Word64 -> Text -> NodeState
updateSystemStartTime ns now systemStart = ns { blockchainMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { systemStartTime = systemStartTimeUTC
    , systemStartTimeChanged = systemStartTime currentMetrics /= systemStartTimeUTC
    }
  currentMetrics = blockchainMetrics ns
  systemStartTimeUTC = read (T.unpack systemStart) :: UTCTime

updateNodePlatform :: NodeState -> Int -> Word64 -> NodeState
updateNodePlatform ns platfId now = ns { nodeMetrics = newNodeMetrics, metricsLastUpdate = now }
 where
  newNodeMetrics = currentMetrics
    { nodePlatform = platformName
    , nodePlatformChanged = True -- nodePlatform currentMetrics /= platformName
    }
  currentMetrics = nodeMetrics ns
  platformName = T.pack . show $ (toEnum platfId :: Platform)

#ifdef LINUX
updateMemoryPages :: NodeState -> Integer -> Word64 -> NodeState
updateMemoryPages ns pages now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics     = currentMetrics { memory = mBytes }
  currentMetrics = resourcesMetrics ns
  mBytes         = fromIntegral (pages * pageSize) / 1024 / 1024 :: Double
  pageSize       = 4096 :: Integer
#endif

#ifdef DARWIN
updateMemoryBytes :: NodeState -> Word64 -> Word64 -> NodeState
updateMemoryBytes ns bytes now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics     = currentMetrics { memory = mBytes }
  currentMetrics = resourcesMetrics ns
  mBytes         = fromIntegral bytes / 1024 / 1024 :: Double
#endif

#ifdef LINUX
updateDiskRead :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateDiskRead ns bytesWereRead meta now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { diskUsageR     = currentDiskRate
      , diskUsageRLast = bytesWereRead
      , diskUsageRNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentDiskRate = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                      then 1.0
                      else bytesDiffInKB'
  bytesDiffInKB'  = bytesDiff / 1024
  bytesDiff       = fromIntegral (bytesWereRead - diskUsageRLast currentMetrics) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  timeDiff        = fromIntegral (currentTimeInNs - diskUsageRNs currentMetrics) :: Double
  currentTimeInNs = utc2ns (tstamp meta)

updateDiskWrite :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateDiskWrite ns bytesWereWritten meta now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { diskUsageW     = currentDiskRate
      , diskUsageWLast = bytesWereWritten
      , diskUsageWNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentDiskRate = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                      then 1.0
                      else bytesDiffInKB'
  bytesDiffInKB'  = bytesDiff / 1024
  bytesDiff       = fromIntegral (bytesWereWritten - diskUsageWLast currentMetrics) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  timeDiff        = fromIntegral (currentTimeInNs - diskUsageWNs currentMetrics) :: Double
  currentTimeInNs = utc2ns (tstamp meta)

updateCPUTicks :: NodeState -> Integer -> LOMeta -> Word64 -> NodeState
updateCPUTicks ns ticks meta now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { cpuPercent = newCPUPercent
      , cpuLast    = ticks
      , cpuNs      = tns
      }
  currentMetrics = resourcesMetrics ns
  newCPUPercent  = if cpuperc < 0
                     then 0.0
                     else cpuperc * 100.0
  cpuperc        = fromIntegral (ticks - cpuLast currentMetrics) / fromIntegral clktck / tdiff
  clktck         = 100 :: Integer
  tdiff          = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000000000 :: Double
  tns            = utc2ns $ tstamp meta
#endif

#if defined(DARWIN) || defined(LINUX)
updateNetworkIn :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateNetworkIn ns inBytes meta now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { networkUsageIn     = currentNetRate
      , networkUsageInLast = inBytes
      , networkUsageInNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = bytesDiff / 1024
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (inBytes - networkUsageInLast currentMetrics) :: Double
  timeDiff        = fromIntegral (currentTimeInNs - networkUsageInNs currentMetrics) :: Double
  currentTimeInNs = utc2ns (tstamp meta)

updateNetworkOut :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateNetworkOut ns outBytes meta now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { networkUsageOut     = currentNetRate
      , networkUsageOutLast = outBytes
      , networkUsageOutNs   = currentTimeInNs
      }
  currentMetrics  = resourcesMetrics ns
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  bytesDiffInKB   = bytesDiff / 1024
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (outBytes - networkUsageOutLast currentMetrics) :: Double
  timeDiff        = fromIntegral (currentTimeInNs - networkUsageOutNs currentMetrics) :: Double
  currentTimeInNs = utc2ns (tstamp meta)
#endif

#if defined(DARWIN) || defined(WINDOWS)
updateCPUSecs :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateCPUSecs ns nanosecs meta now = ns { resourcesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics =
    currentMetrics
      { cpuPercent = newCPUPercent
      , cpuLast    = fromIntegral nanosecs
      , cpuNs      = tns
      }
  currentMetrics = resourcesMetrics ns
  newCPUPercent  = if cpuperc < 0
                    then 0.0
                    else if cpuperc > 20.0
                           then cpuPercent currentMetrics
                           else cpuperc * 100.0
  cpuperc  = fromIntegral deltacpu / 100000000 / tdiff
  deltacpu = fromIntegral nanosecs - cpuLast currentMetrics
  tdiff    = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000000000 :: Double
  tns      = utc2ns $ tstamp meta
#endif

updateMempoolTxs :: NodeState -> Integer -> Word64 -> NodeState
updateMempoolTxs ns txsInMempool now = ns { mempoolMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { mempoolTxsNumber = txsInMempool
    , mempoolTxsNumberChanged = changed
    , mempoolTxsPercent =   fromIntegral txsInMempool
                          / fromIntegral maxTxs
                          * 100.0
    , mempoolTxsPercentChanged = changed
    , mempoolMaxTxs = maxTxs
    , mempoolMaxTxsChanged = mempoolMaxTxs currentMetrics /= maxTxs
    }
  currentMetrics = mempoolMetrics ns
  currentMempoolTxsNumber = mempoolTxsNumber currentMetrics
  maxTxs = max txsInMempool (mempoolMaxTxs currentMetrics)
  changed = currentMempoolTxsNumber /= fromIntegral txsInMempool

updateMempoolBytes :: NodeState -> Integer -> Word64 -> NodeState
updateMempoolBytes ns newMempoolBytes now = ns { mempoolMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { mempoolBytes = fromIntegral newMempoolBytes
    , mempoolBytesChanged = currentMempoolBytes /= fromIntegral newMempoolBytes
    , mempoolBytesPercent =   fromIntegral newMempoolBytes
                            / fromIntegral maxBytes
                            * 100.0
    , mempoolMaxBytes = maxBytes
    , mempoolMaxBytesChanged = mempoolMaxBytes currentMetrics /= maxBytes
    }
  currentMetrics = mempoolMetrics ns
  currentMempoolBytes = mempoolBytes currentMetrics
  maxBytes = max newMempoolBytes (mempoolMaxBytes currentMetrics)

updateTxsProcessed :: NodeState -> Integer -> Word64 -> NodeState
updateTxsProcessed ns txsProcNum now = ns { mempoolMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { txsProcessed = txsProcNum
    , txsProcessedChanged = txsProcessed currentMetrics /= txsProcNum
    }
  currentMetrics = mempoolMetrics ns

updateBlocksForged :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksForged ns blocksForged now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { blocksForgedNumber = blocksForged
    , blocksForgedNumberChanged = blocksForgedNumber currentMetrics /= blocksForged
    }
  currentMetrics = forgeMetrics ns

updateNodeCannotForge :: NodeState -> Integer -> Word64 -> NodeState
updateNodeCannotForge ns cannotForge now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { nodeCannotForge = cannotForge
    , nodeCannotForgeChanged = nodeCannotForge currentMetrics /= cannotForge
    }
  currentMetrics = forgeMetrics ns

updateNodeIsLeader :: NodeState -> Integer -> Word64 -> NodeState
updateNodeIsLeader ns nodeIsLeader now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { nodeIsLeaderNum = nodeIsLeader
    , nodeIsLeaderNumChanged = nodeIsLeaderNum currentMetrics /= nodeIsLeader
    }
  currentMetrics = forgeMetrics ns

updateSlotsMissed :: NodeState -> Integer -> Word64 -> NodeState
updateSlotsMissed ns slotsMissed now = ns { forgeMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { slotsMissedNumber = slotsMissed
    , slotsMissedNumberChanged = slotsMissedNumber currentMetrics /= slotsMissed
    }
  currentMetrics = forgeMetrics ns

updateRTSBytesAllocated :: NodeState -> Word64 -> Word64 -> NodeState
updateRTSBytesAllocated ns bytesAllocated now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsMemoryAllocated  = newRTSMemoryAllocated
    , rtsMemoryAllocatedChanged = rtsMemoryAllocated currentMetrics /= newRTSMemoryAllocated
    }
  currentMetrics = rtsMetrics ns
  newRTSMemoryAllocated = fromIntegral bytesAllocated / 1024 / 1024 :: Double

updateRTSBytesUsed :: NodeState -> Word64 -> Word64 -> NodeState
updateRTSBytesUsed ns usedMemBytes now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsMemoryUsed = mBytes
    , rtsMemoryUsedChanged = changed
    , rtsMemoryUsedPercent =   mBytes
                             / rtsMemoryAllocated currentMetrics
                             * 100.0
    , rtsMemoryUsedPercentChanged = changed
    }
  currentMetrics = rtsMetrics ns
  mBytes = fromIntegral usedMemBytes / 1024 / 1024 :: Double
  changed = rtsMemoryUsed currentMetrics /= mBytes

updateGcCpuNs :: NodeState -> Word64 -> Word64 -> NodeState
updateGcCpuNs ns gcCpuNs now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsGcCpu = newGcCpu
    , rtsGcCpuChanged = rtsGcCpu currentMetrics /= newGcCpu
    }
  currentMetrics = rtsMetrics ns
  newGcCpu = fromIntegral gcCpuNs / 1000000000 :: Double

updateGcElapsedNs :: NodeState -> Word64 -> Word64 -> NodeState
updateGcElapsedNs ns gcElapsedNs now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsGcElapsed = newGcElapsed
    , rtsGcElapsedChanged = rtsGcElapsed currentMetrics /= newGcElapsed
    }
  currentMetrics = rtsMetrics ns
  newGcElapsed = fromIntegral gcElapsedNs / 1000000000 :: Double

updateGcNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcNum ns gcNum now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsGcNum = gcNum
    , rtsGcNumChanged = rtsGcNum currentMetrics /= gcNum
    }
  currentMetrics = rtsMetrics ns

updateGcMajorNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcMajorNum ns gcMajorNum now = ns { rtsMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { rtsGcMajorNum = gcMajorNum
    , rtsGcMajorNumChanged = rtsGcMajorNum currentMetrics /= gcMajorNum
    }
  currentMetrics = rtsMetrics ns

updateCertStartKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertStartKESPeriod ns oCertStartKesPeriod now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { opCertStartKESPeriod = oCertStartKesPeriod
    , opCertStartKESPeriodChanged = opCertStartKESPeriod currentMetrics /= oCertStartKesPeriod
    }
  currentMetrics = kesMetrics ns

updateCertExpiryKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { opCertExpiryKESPeriod = oCertExpiryKesPeriod
    , opCertExpiryKESPeriodChanged = opCertExpiryKESPeriod currentMetrics /= oCertExpiryKesPeriod
    }
  currentMetrics = kesMetrics ns

updateCurrentKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCurrentKESPeriod ns currentKesPeriod now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { currentKESPeriod = currentKesPeriod
    , currentKESPeriodChanged = currentKESPeriod currentMetrics /= currentKesPeriod
    }
  currentMetrics = kesMetrics ns

updateRemainingKESPeriods :: NodeState -> Integer -> Word64 -> NodeState
updateRemainingKESPeriods ns kesPeriodsUntilExpiry now = ns { kesMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { remKESPeriods = kesPeriodsUntilExpiry
    , remKESPeriodsChanged = changed
    , remKESPeriodsInDays = floor remainingInDays
    , remKESPeriodsInDaysChanged = changed
    }
  currentMetrics = kesMetrics ns
  changed = remKESPeriods currentMetrics /= kesPeriodsUntilExpiry
  remainingInSeconds =   fromIntegral kesPeriodsUntilExpiry
                       * (1 :: Int)      -- TODO: take from node basic info metric for current protocol.
                       * (129600 :: Int) -- TODO: take from node basic info metric for current protocol.
  remainingInDays :: Double
  remainingInDays = fromIntegral remainingInSeconds / 3600 / 24

updateChainDensity :: NodeState -> Double -> Word64 -> NodeState
updateChainDensity ns density now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { chainDensity = newDencity
    , chainDensityChanged = chainDensity currentMetrics /= newDencity
    }
  currentMetrics = blockchainMetrics ns
  newDencity = 0.05 + density * 100.0

updateBlocksNumber :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksNumber ns blockNum now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { blocksNumber = blockNum
    , blocksNumberChanged = blocksNumber currentMetrics /= blockNum
    }
  currentMetrics = blockchainMetrics ns

updateSlotInEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateSlotInEpoch ns slotNum now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { slot = slotNum
    , slotChanged = slot currentMetrics /= slotNum
    }
  currentMetrics = blockchainMetrics ns

updateEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateEpoch ns newEpoch now = ns { blockchainMetrics = newMetrics, metricsLastUpdate = now }
 where
  newMetrics = currentMetrics
    { epoch = newEpoch
    , epochChanged = epoch currentMetrics /= newEpoch
    }
  currentMetrics = blockchainMetrics ns
