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
import           Data.List ((!!))
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
import           Cardano.RTView.NodeState.Types (NodeError (..), NodeInfo (..),
                                                 NodeMetrics (..), NodeState (..),
                                                 NodesState)

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
              LogStructured peersInfo ->
                nodesStateWith $ updatePeersInfo ns peersInfo
              _ -> return currentNodesState
           | "cardano.node.metrics" `T.isInfixOf` aName ->
            case aContent of
              LogValue "upTime" (Nanoseconds upTimeInNs) ->
                nodesStateWith $ updateNodeUpTime ns upTimeInNs now
              LogValue "txsInMempool" (PureI txsInMempool) ->
                nodesStateWith $ updateMempoolTxs ns txsInMempool
              LogValue "mempoolBytes" (PureI mempoolBytes) ->
                nodesStateWith $ updateMempoolBytes ns mempoolBytes
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
              LogValue "epoch" (PureI epoch) ->
                nodesStateWith $ updateEpoch ns epoch now
              _ -> return currentNodesState
      Nothing ->
        -- This is a problem, because it means that configuration is unexpected one:
        -- name of node in getAcceptAt doesn't correspond to the name of loggerName.
        return currentNodesState

-- Updaters for particular node state's fields.

updateNodeUpTime :: NodeState -> Word64 -> Word64 -> NodeState
updateNodeUpTime ns upTimeInNs now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niUpTime = upTimeInNs
      , niUpTimeLastUpdate = now
      }
  currentNi = nsInfo ns

updateNodeErrors :: Show a => NodeState -> LOMeta -> LOContent a -> NodeState
updateNodeErrors ns (LOMeta timeStamp _ _ sev _) aContent = ns { nsInfo = newNi }
 where
  newNi = currentNi { niNodeErrors = NodeError timeStamp sev errorMessage : currentErrors }
  currentNi = nsInfo ns
  currentErrors = niNodeErrors currentNi
  errorMessage =
    case aContent of
      LogMessage msg -> show msg
      LogError eMsg -> T.unpack eMsg
      LogStructured obj -> show obj
      LogStructuredText _ txt -> T.unpack txt
      MonitoringEffect (MonitorAlert msg) -> "Monitor alert: " <> T.unpack msg
      MonitoringEffect _ -> ""
      _ -> "UNPARSED_ERROR_MESSAGE"

updatePeersInfo :: NodeState -> A.Object -> NodeState
updatePeersInfo ns peersInfo = ns { nsInfo = newNi }
 where
  newNi = currentNi { niPeersInfo = newPeersInfo }
  currentNi = nsInfo ns
  newPeersInfo = extractPeersInfo peersInfo

updateNodeProtocol :: NodeState -> Text -> NodeState
updateNodeProtocol ns protocol = ns { nsInfo = newNi }
 where
  newNi = currentNi { niNodeProtocol = T.unpack protocol }
  currentNi = nsInfo ns

updateNodeVersion :: NodeState -> Text -> NodeState
updateNodeVersion ns version = ns { nsInfo = newNi }
 where
  newNi = currentNi { niNodeVersion = T.unpack version }
  currentNi = nsInfo ns

updateNodeCommit :: NodeState -> Text -> NodeState
updateNodeCommit ns commit = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niNodeCommit      = T.unpack commit
      , niNodeShortCommit = take 7 $ T.unpack commit
      }
  currentNi = nsInfo ns

updateNodePlatform :: NodeState -> Int -> NodeState
updateNodePlatform ns platfid = ns { nsInfo = newNi }
 where
  platform = toEnum platfid :: Platform
  newNi = currentNi { niNodePlatform = show platform }
  currentNi = nsInfo ns

#ifdef LINUX
updateMemoryPages :: NodeState -> Integer -> Word64 -> NodeState
updateMemoryPages ns pages now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMemory         = mBytes
      , nmMemoryMax      = newMax
      , nmMemoryMaxTotal = newMaxTotal
      , nmMemoryPercent  = mBytes / newMaxTotal * 100.0
      , nmMemoryLastUpdate = now
      }
  currentNm   = nsMetrics ns
  prevMax     = nmMemoryMax currentNm
  newMax      = max prevMax mBytes
  newMaxTotal = max newMax 200.0
  mBytes      = fromIntegral (pages * pageSize) / 1024 / 1024 :: Double
  pageSize    = 4096 :: Integer
#endif

#ifdef DARWIN
updateMemoryBytes :: NodeState -> Word64 -> Word64 -> NodeState
updateMemoryBytes ns bytes now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMemory         = mBytes
      , nmMemoryMax      = newMax
      , nmMemoryMaxTotal = newMaxTotal
      , nmMemoryPercent  = mBytes / newMaxTotal * 100.0
      , nmMemoryLastUpdate = now
      }
  currentNm   = nsMetrics ns
  prevMax     = nmMemoryMax currentNm
  newMax      = max prevMax mBytes
  newMaxTotal = max newMax 200.0
  mBytes      = fromIntegral bytes / 1024 / 1024 :: Double
#endif

#ifdef LINUX
updateDiskRead :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateDiskRead ns bytesWereRead meta now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmDiskUsageR           = currentDiskRate
      , nmDiskUsageRPercent    = diskUsageRPercent
      , nmDiskUsageRLast       = bytesWereRead
      , nmDiskUsageRNs         = currentTimeInNs
      , nmDiskUsageRMax        = maxDiskRate
      , nmDiskUsageRMaxTotal   = max maxDiskRate 1.0
      , nmDiskUsageRAdaptTime  = newAdaptTime
      , nmDiskUsageRLastUpdate = now
      }
  currentNm         = nsMetrics ns
  currentTimeInNs   = utc2ns (tstamp meta)
  timeDiff          = fromIntegral (currentTimeInNs - nmDiskUsageRNs currentNm) :: Double
  timeDiffInSecs    = timeDiff / 1000000000
  bytesDiff         = fromIntegral (bytesWereRead - nmDiskUsageRLast currentNm) :: Double
  bytesDiffInKB'    = bytesDiff / 1024
  bytesDiffInKB     = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                        then 1.0
                        else bytesDiffInKB'
  currentDiskRate   = bytesDiffInKB / timeDiffInSecs
  lastAdaptTime     = nmDiskUsageRAdaptTime currentNm
  timeElapsed       = diffUTCTime (tstamp meta) lastAdaptTime
  ( maxDiskRate
    , newAdaptTime ) =
        if timeElapsed >= adaptPeriod
          then ((nmDiskUsageRMax currentNm + currentDiskRate) / 2, tstamp meta)
          else (max currentDiskRate $ nmDiskUsageRMax currentNm, lastAdaptTime)
  diskUsageRPercent = currentDiskRate / (maxDiskRate / 100.0)

updateDiskWrite :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateDiskWrite ns bytesWereWritten meta now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmDiskUsageW           = currentDiskRate
      , nmDiskUsageWPercent    = diskUsageWPercent
      , nmDiskUsageWLast       = bytesWereWritten
      , nmDiskUsageWNs         = currentTimeInNs
      , nmDiskUsageWMax        = maxDiskRate
      , nmDiskUsageWMaxTotal   = max maxDiskRate 1.0
      , nmDiskUsageWAdaptTime  = newAdaptTime
      , nmDiskUsageWLastUpdate = now
      }
  currentNm         = nsMetrics ns
  currentTimeInNs   = utc2ns (tstamp meta)
  timeDiff          = fromIntegral (currentTimeInNs - nmDiskUsageWNs currentNm) :: Double
  timeDiffInSecs    = timeDiff / 1000000000
  bytesDiff         = fromIntegral (bytesWereWritten - nmDiskUsageWLast currentNm) :: Double
  bytesDiffInKB'    = bytesDiff / 1024
  bytesDiffInKB     = if bytesDiffInKB' > 500.0 -- To prevent an overflow if the node was restarted.
                        then 1.0
                        else bytesDiffInKB'
  currentDiskRate   = bytesDiffInKB / timeDiffInSecs
  lastAdaptTime     = nmDiskUsageWAdaptTime currentNm
  timeElapsed       = diffUTCTime (tstamp meta) lastAdaptTime
  ( maxDiskRate
    , newAdaptTime ) =
        if timeElapsed >= adaptPeriod
          then ((nmDiskUsageWMax currentNm + currentDiskRate) / 2, tstamp meta)
          else (max currentDiskRate $ nmDiskUsageWMax currentNm, lastAdaptTime)
  diskUsageWPercent = currentDiskRate / (maxDiskRate / 100.0)

-- | Adaptaion period for disk usage max values.
--   We have to adapt the max value to the new situation periodically,
--   because it might get very high once, and then it will stay there forever.
adaptPeriod :: NominalDiffTime
adaptPeriod = fromInteger $ 60 * 2 -- 2 minutes.

updateCPUTicks :: NodeState -> Integer -> LOMeta -> Word64 -> NodeState
updateCPUTicks ns ticks meta now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmCPUPercent    = cpuperc * 100.0
      , nmCPULast       = ticks
      , nmCPUNs         = tns
      , nmCPULastUpdate = now
      }
  currentNm = nsMetrics ns
  tns       = utc2ns $ tstamp meta
  tdiff     = max 0.1 $ fromIntegral (tns - nmCPUNs currentNm) / 1000000000 :: Double
  cpuperc   = fromIntegral (ticks - nmCPULast currentNm) / fromIntegral clktck / tdiff
  clktck    = 100 :: Integer
#endif

#if defined(DARWIN) || defined(LINUX)
updateNetworkIn :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateNetworkIn ns inBytes meta now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmNetworkUsageIn           = currentNetRate
      , nmNetworkUsageInPercent    = currentNetRate / (maxNetRate / 100.0)
      , nmNetworkUsageInLast       = inBytes
      , nmNetworkUsageInNs         = currentTimeInNs
      , nmNetworkUsageInMax        = maxNetRate
      , nmNetworkUsageInMaxTotal   = max maxNetRate 1.0
      , nmNetworkUsageInLastUpdate = now
      }
  currentNm       = nsMetrics ns
  currentTimeInNs = utc2ns (tstamp meta)
  timeDiff        = fromIntegral (currentTimeInNs - nmNetworkUsageInNs currentNm) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (inBytes - nmNetworkUsageInLast currentNm) :: Double
  bytesDiffInKB   = bytesDiff / 1024
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  maxNetRate      = max currentNetRate $ nmNetworkUsageInMax currentNm

updateNetworkOut :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateNetworkOut ns outBytes meta now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmNetworkUsageOut           = currentNetRate
      , nmNetworkUsageOutPercent    = currentNetRate / (maxNetRate / 100.0)
      , nmNetworkUsageOutLast       = outBytes
      , nmNetworkUsageOutNs         = currentTimeInNs
      , nmNetworkUsageOutMax        = maxNetRate
      , nmNetworkUsageOutMaxTotal   = max maxNetRate 1.0
      , nmNetworkUsageOutLastUpdate = now
      }
  currentNm       = nsMetrics ns
  currentTimeInNs = utc2ns (tstamp meta)
  timeDiff        = fromIntegral (currentTimeInNs - nmNetworkUsageOutNs currentNm) :: Double
  timeDiffInSecs  = timeDiff / 1000000000
  bytesDiff       = fromIntegral (outBytes - nmNetworkUsageOutLast currentNm) :: Double
  bytesDiffInKB   = bytesDiff / 1024
  currentNetRate  = bytesDiffInKB / timeDiffInSecs
  maxNetRate      = max currentNetRate $ nmNetworkUsageOutMax currentNm
#endif

#if defined(DARWIN) || defined(WINDOWS)
updateCPUSecs :: NodeState -> Word64 -> LOMeta -> Word64 -> NodeState
updateCPUSecs ns nanosecs meta now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmCPUPercent    = if cpuperc < 0 then 0 else if cpuperc > 20.0 then nmCPUPercent currentNm else cpuperc * 100.0
      , nmCPULast       = fromIntegral nanosecs
      , nmCPUNs         = tns
      , nmCPULastUpdate = now
      }
  currentNm = nsMetrics ns
  tns       = utc2ns $ tstamp meta
  tdiff     = max 0.1 $ fromIntegral (tns - nmCPUNs currentNm) / 1000000000 :: Double
  deltacpu  = fromIntegral nanosecs - nmCPULast currentNm
  cpuperc   = fromIntegral deltacpu / 100000000 / tdiff
#endif

updateMempoolTxs :: NodeState -> Integer -> NodeState
updateMempoolTxs ns txsInMempool = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMempoolTxsNumber  = fromIntegral txsInMempool
      , nmMempoolTxsPercent =   fromIntegral txsInMempool
                              / fromIntegral maxTxs
                              * 100.0
      , nmMempoolMaxTxs = maxTxs
      }
  currentNm = nsMetrics ns
  maxTxs = max txsInMempool (nmMempoolMaxTxs currentNm)

updateMempoolBytes :: NodeState -> Integer -> NodeState
updateMempoolBytes ns mempoolBytes = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmMempoolBytes = fromIntegral mempoolBytes
      , nmMempoolBytesPercent =   fromIntegral mempoolBytes
                                / fromIntegral maxBytes
                                * 100.0
      , nmMempoolMaxBytes = maxBytes
      }
  currentNm = nsMetrics ns
  maxBytes = max mempoolBytes (nmMempoolMaxBytes currentNm)

updateTxsProcessed :: NodeState -> Integer -> NodeState
updateTxsProcessed ns txsProcessed = ns { nsInfo = newNi }
 where
  newNi = currentNi { niTxsProcessed = txsProcessed }
  currentNi = nsInfo ns

updateBlocksForged :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksForged ns blocksForged now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niBlocksForgedNumber = blocksForged
      , niBlocksForgedNumberLastUpdate = now
      }
  currentNi = nsInfo ns

updateNodeCannotForge :: NodeState -> Integer -> NodeState
updateNodeCannotForge ns cannotForge = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niNodeCannotForge = cannotForge
      }
  currentNi = nsInfo ns

updateNodeIsLeader :: NodeState -> Integer -> Word64 -> NodeState
updateNodeIsLeader ns nodeIsLeader now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niNodeIsLeaderNum = nodeIsLeader
      , niNodeIsLeaderNumLastUpdate = now
      }
  currentNi = nsInfo ns

updateSlotsMissed :: NodeState -> Integer -> Word64 -> NodeState
updateSlotsMissed ns slotsMissed now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niSlotsMissedNumber = slotsMissed
      , niSlotsMissedNumberLastUpdate = now
      }
  currentNi = nsInfo ns

updateRTSBytesAllocated :: NodeState -> Word64 -> Word64 -> NodeState
updateRTSBytesAllocated ns bytesAllocated now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmRTSMemoryAllocated = mBytes
      , nmRTSMemoryLastUpdate = now
      }
  currentNm = nsMetrics ns
  mBytes    = fromIntegral bytesAllocated / 1024 / 1024 :: Double

updateRTSBytesUsed :: NodeState -> Word64 -> Word64 -> NodeState
updateRTSBytesUsed ns usedMemBytes now = ns { nsMetrics = newNm }
 where
  newNm =
    currentNm
      { nmRTSMemoryUsed = mBytes
      , nmRTSMemoryUsedPercent =   mBytes
                                 / nmRTSMemoryAllocated currentNm
                                 * 100.0
      , nmRTSMemoryLastUpdate = now
      }
  currentNm = nsMetrics ns
  mBytes    = fromIntegral usedMemBytes / 1024 / 1024 :: Double

updateGcCpuNs :: NodeState -> Word64 -> Word64 -> NodeState
updateGcCpuNs ns gcCpuNs now = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcCpu = seconds
                        , nmRTSGcCpuLastUpdate = now
                        }
  currentNm = nsMetrics ns
  seconds   = fromIntegral gcCpuNs / 1000000000 :: Double

updateGcElapsedNs :: NodeState -> Word64 -> Word64 -> NodeState
updateGcElapsedNs ns gcElapsedNs now = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcElapsed = seconds
                        , nmRTSGcElapsedLastUpdate = now
                        }
  currentNm = nsMetrics ns
  seconds   = fromIntegral gcElapsedNs / 1000000000 :: Double

updateGcNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcNum ns gcNum now = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcNum = gcNum
                        , nmRTSGcNumLastUpdate = now
                        }
  currentNm = nsMetrics ns

updateGcMajorNum :: NodeState -> Integer -> Word64 -> NodeState
updateGcMajorNum ns gcMajorNum now = ns { nsMetrics = newNm }
 where
  newNm     = currentNm { nmRTSGcMajorNum = gcMajorNum
                        , nmRTSGcMajorNumLastUpdate = now
                        }
  currentNm = nsMetrics ns

updateCertStartKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertStartKESPeriod ns oCertStartKesPeriod now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niOpCertStartKESPeriod = oCertStartKesPeriod
      , niOpCertStartKESPeriodLastUpdate = now
      }
  currentNi = nsInfo ns

updateCertExpiryKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCertExpiryKESPeriod ns oCertExpiryKesPeriod now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niOpCertExpiryKESPeriod = oCertExpiryKesPeriod
      , niOpCertExpiryKESPeriodLastUpdate = now
      }
  currentNi = nsInfo ns

updateCurrentKESPeriod :: NodeState -> Integer -> Word64 -> NodeState
updateCurrentKESPeriod ns currentKesPeriod now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niCurrentKESPeriod = currentKesPeriod
      , niCurrentKESPeriodLastUpdate = now
      }
  currentNi = nsInfo ns

updateRemainingKESPeriods :: NodeState -> RTViewParams -> Integer -> Word64 -> NodeState
updateRemainingKESPeriods ns params kesPeriodsUntilExpiry now = ns { nsInfo = newNi }
 where
  newNi =
    currentNi
      { niRemainingKESPeriods = kesPeriodsUntilExpiry
      , niRemainingKESPeriodsInDays = floor remainingInDays
      , niRemainingKESPeriodsLastUpdate = now
      }
  currentNi = nsInfo ns
  remainingInSeconds =   fromIntegral kesPeriodsUntilExpiry
                       * rtvSlotLength params
                       * rtvSlotsPerKESPeriod params
  remainingInDays :: Double
  remainingInDays = fromIntegral remainingInSeconds / 3600 / 24

updateChainDensity :: NodeState -> Double -> Word64 -> NodeState
updateChainDensity ns density now = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niChainDensity = chainDensity
                      , niChainDensityLastUpdate = now
                      }
  chainDensity = 0.05 + density * 100.0

updateBlocksNumber :: NodeState -> Integer -> Word64 -> NodeState
updateBlocksNumber ns blockNum now = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niBlocksNumber = blockNum
                      , niBlocksNumberLastUpdate = now
                      }

updateSlotInEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateSlotInEpoch ns slotNum now = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niSlot = slotNum
                      , niSlotLastUpdate = now
                      }

updateEpoch :: NodeState -> Integer -> Word64 -> NodeState
updateEpoch ns epoch now = ns { nsInfo = newNi }
 where
  newNi = (nsInfo ns) { niEpoch = epoch
                      , niEpochLastUpdate = now
                      }
