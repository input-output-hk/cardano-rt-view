{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.GUI.Updater
    ( updateGUI
    ) where

import           Control.Monad (void, forM, forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import           Data.Maybe (isJust)
import           Data.Map.Strict ((!))
import           Data.Text (Text, pack, strip, unpack)
import           Data.Time.Calendar (Day (..), diffDays)
import           Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime,
                                  diffUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Word (Word64)
import           Formatting (fixed, sformat, (%))
import           GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, children, element, set, style, text,
                                              (#), (#+))

import           Cardano.BM.Data.Configuration (RemoteAddr (..), RemoteAddrNamed (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.GUI.Elements (ElementName (..), ElementValue (..),
                                              HTMLClass (..), HTMLId (..),
                                              NodesStateElements,
                                              PeerInfoElements (..), PeerInfoItem (..),
                                              (#.))
import           Cardano.RTView.GUI.Markup.Grid (allMetricsNames)
import qualified Cardano.RTView.GUI.JS.Charts as Chart
import           Cardano.RTView.NodeState.Types
import           Cardano.RTView.SupportedNodes (supportedNodesVersions, showSupportedNodesVersions)

-- | This function is calling by the timer. It updates the node' state elements
--   on the page automatically, because threepenny-gui is based on websockets.
updateGUI
  :: UI.Window
  -> NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> (NodesStateElements, NodesStateElements)
  -> UI ()
updateGUI window nodesState params acceptors (nodesStateElems, gridNodesStateElems) = do
  -- Only one GUI mode can be active now, so check it and update only corresponding elements.
  UI.getElementById window (show ViewModeButton) >>= \case
    Just btn -> UI.get UI.value btn >>= \case
      "paneMode" -> updatePaneGUI window nodesState params acceptors nodesStateElems
      _ ->          updateGridGUI window nodesState params acceptors gridNodesStateElems
    Nothing -> return ()

updatePaneGUI
  :: UI.Window
  -> NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> NodesStateElements
  -> UI ()
updatePaneGUI window nodesState params acceptors nodesStateElems =
  forM_ nodesStateElems $ \(nameOfNode, els, peerInfoItems) -> do
    let NodeState {..} = nodesState ! nameOfNode
        acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

    let PeerMetrics {..}         = peersMetrics
        MempoolMetrics {..}      = mempoolMetrics
        ForgeMetrics {..}        = forgeMetrics
        rm@ResourcesMetrics {..} = resourcesMetrics
        RTSMetrics {..}          = rtsMetrics
        BlockchainMetrics {..}   = blockchainMetrics
        KESMetrics {..}          = kesMetrics
        nm@NodeMetrics {..}      = nodeMetrics
        ErrorsMetrics {..}       = nodeErrors

    nodeIsIdle <- checkIfNodeIsIdlePane params metricsLastUpdate (els ! ElIdleNode) (els ! ElNodePane)
    unless nodeIsIdle $ do
      updateNodeVersion nodeVersion nodeVersionChanged $ els ! ElNodeVersion

      updateElement (ElementText    nodeProtocol)         nodeProtocolChanged       $ els ! ElNodeProtocol
      updateElement (ElementText    nodePlatform)         nodePlatformChanged       $ els ! ElNodePlatform
      updateElement (ElementInteger epoch)                epochChanged              $ els ! ElEpoch
      updateElement (ElementInteger slot)                 slotChanged               $ els ! ElSlot
      updateElement (ElementInteger blocksNumber)         blocksNumberChanged       $ els ! ElBlocksNumber
      updateElement (ElementDouble  chainDensity)         chainDensityChanged       $ els ! ElChainDensity
      updateElement (ElementInteger blocksForgedNumber)   blocksForgedNumberChanged $ els ! ElBlocksForgedNumber
      updateElement (ElementInteger nodeCannotForge)      nodeCannotForgeChanged    $ els ! ElNodeCannotForge
      updateElement (ElementInteger nodeIsLeaderNum)      nodeIsLeaderNumChanged    $ els ! ElNodeIsLeaderNumber
      updateElement (ElementInteger slotsMissedNumber)    slotsMissedNumberChanged  $ els ! ElSlotsMissedNumber
      updateElement (ElementInteger txsProcessed)         txsProcessedChanged       $ els ! ElTxsProcessed
      updateElement (ElementWord64  mempoolTxsNumber)     mempoolTxsNumberChanged   $ els ! ElMempoolTxsNumber
      updateElement (ElementDouble  mempoolTxsPercent)    mempoolTxsNumberChanged   $ els ! ElMempoolTxsPercent
      updateElement (ElementWord64  mempoolBytes)         mempoolBytesChanged       $ els ! ElMempoolBytes
      updateElement (ElementDouble  mempoolBytesPercent)  mempoolBytesChanged       $ els ! ElMempoolBytesPercent
      updateElement (ElementInteger mempoolMaxTxs)        True                      $ els ! ElMempoolMaxTxs
      updateElement (ElementInteger mempoolMaxBytes)      True                      $ els ! ElMempoolMaxBytes
      updateElement (ElementDouble  diskUsageR)           diskUsageRChanged         $ els ! ElDiskUsageR
      updateElement (ElementDouble  diskUsageW)           diskUsageWChanged         $ els ! ElDiskUsageW
      updateElement (ElementDouble  networkUsageIn)       networkUsageInChanged     $ els ! ElNetworkUsageIn
      updateElement (ElementDouble  networkUsageOut)      networkUsageOutChanged    $ els ! ElNetworkUsageOut
      updateElement (ElementDouble  rtsMemoryAllocated)   rtsMemoryAllocatedChanged $ els ! ElRTSMemoryAllocated
      updateElement (ElementDouble  rtsMemoryUsed)        rtsMemoryUsedChanged      $ els ! ElRTSMemoryUsed
      updateElement (ElementDouble  rtsMemoryUsedPercent) rtsMemoryUsedChanged      $ els ! ElRTSMemoryUsedPercent
      updateElement (ElementDouble  rtsGcCpu)             rtsGcCpuChanged           $ els ! ElRTSGcCpu
      updateElement (ElementDouble  rtsGcElapsed)         rtsGcElapsedChanged       $ els ! ElRTSGcElapsed
      updateElement (ElementInteger rtsGcNum)             rtsGcNumChanged           $ els ! ElRTSGcNum
      updateElement (ElementInteger rtsGcMajorNum)        rtsGcMajorNumChanged      $ els ! ElRTSGcMajorNum

      updateSystemStart systemStartTime $ els ! ElSystemStartTime

      updateCharts window nameOfNode rm nm

      updateNodeUpTime nodeStartTime $ els ! ElUptime
      updateNodeCommit nodeCommit
                       nodeShortCommit
                       nodeCommitChanged    $ els ! ElNodeCommitHref
      updateEndpoint   acceptorEndpoint     $ els ! ElTraceAcceptorEndpoint
      updateErrorsList errors errorsChanged $ els ! ElNodeErrors
      updateErrorsTab  errors errorsChanged $ els ! ElNodeErrorsTab

      updateKESInfo [ (opCertStartKESPeriod,      els ! ElOpCertStartKESPeriod)
                    , (opCertExpiryKESPeriod,     els ! ElOpCertExpiryKESPeriod)
                    , (currentKESPeriod,          els ! ElCurrentKESPeriod)
                    , (remainingKESPeriods,       els ! ElRemainingKESPeriods)
                    , (remainingKESPeriodsInDays, els ! ElRemainingKESPeriodsInDays)
                    ]

      updatePeersList peersInfo peersInfoChanged peerInfoItems

      updateProgressBar mempoolBytesPercent  $ els ! ElMempoolBytesProgress
      updateProgressBar mempoolTxsPercent    $ els ! ElMempoolTxsProgress
      updateProgressBar rtsMemoryUsedPercent $ els ! ElRTSMemoryProgress

updateGridGUI
  :: UI.Window
  -> NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> NodesStateElements
  -> UI ()
updateGridGUI window nodesState params acceptors gridNodesStateElems =
  forM_ gridNodesStateElems $ \(nameOfNode, els, _) -> do
    let NodeState {..} = nodesState ! nameOfNode
        acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

    let PeerMetrics {..}       = peersMetrics
        MempoolMetrics {..}    = mempoolMetrics
        ForgeMetrics {..}      = forgeMetrics
        rm                     = resourcesMetrics
        RTSMetrics {..}        = rtsMetrics
        BlockchainMetrics {..} = blockchainMetrics
        KESMetrics {..}        = kesMetrics
        nm@NodeMetrics {..}    = nodeMetrics

    nodeIsIdle <- checkIfNodeIsIdleGrid window params metricsLastUpdate (els ! ElIdleNode) nameOfNode
    unless nodeIsIdle $ do
      updateNodeVersion nodeVersion nodeVersionChanged $ els ! ElNodeVersion

      updateElement (ElementText    nodeProtocol)       nodeProtocolChanged       $ els ! ElNodeProtocol
      updateElement (ElementText    nodePlatform)       nodePlatformChanged       $ els ! ElNodePlatform
      updateElement (ElementInt     $ length peersInfo) peersInfoChanged          $ els ! ElPeersNumber
      updateElement (ElementInteger epoch)              epochChanged              $ els ! ElEpoch
      updateElement (ElementInteger slot)               slotChanged               $ els ! ElSlot
      updateElement (ElementInteger blocksNumber)       blocksNumberChanged       $ els ! ElBlocksNumber
      updateElement (ElementDouble  chainDensity)       chainDensityChanged       $ els ! ElChainDensity
      updateElement (ElementInteger blocksForgedNumber) blocksForgedNumberChanged $ els ! ElBlocksForgedNumber
      updateElement (ElementInteger nodeCannotForge)    nodeCannotForgeChanged    $ els ! ElNodeCannotForge
      updateElement (ElementInteger nodeIsLeaderNum)    nodeIsLeaderNumChanged    $ els ! ElNodeIsLeaderNumber
      updateElement (ElementInteger slotsMissedNumber)  slotsMissedNumberChanged  $ els ! ElSlotsMissedNumber
      updateElement (ElementInteger txsProcessed)       txsProcessedChanged       $ els ! ElTxsProcessed
      updateElement (ElementWord64  mempoolTxsNumber)   mempoolTxsNumberChanged   $ els ! ElMempoolTxsNumber
      updateElement (ElementWord64  mempoolBytes)       mempoolBytesChanged       $ els ! ElMempoolBytes
      updateElement (ElementDouble  rtsGcCpu)           rtsGcCpuChanged           $ els ! ElRTSGcCpu
      updateElement (ElementDouble  rtsGcElapsed)       rtsGcElapsedChanged       $ els ! ElRTSGcElapsed
      updateElement (ElementInteger rtsGcNum)           rtsGcNumChanged           $ els ! ElRTSGcNum
      updateElement (ElementInteger rtsGcMajorNum)      rtsGcMajorNumChanged      $ els ! ElRTSGcMajorNum

      updateSystemStart systemStartTime $ els ! ElSystemStartTime

      updateCharts window nameOfNode rm nm

      updateEndpoint   acceptorEndpoint  $ els ! ElTraceAcceptorEndpoint
      updateNodeCommit nodeCommit
                       nodeShortCommit
                       nodeCommitChanged $ els ! ElNodeCommitHref
      updateNodeUpTime nodeStartTime $ els ! ElUptime

      updateKESInfo [ (opCertStartKESPeriod,      els ! ElOpCertStartKESPeriod)
                    , (opCertExpiryKESPeriod,     els ! ElOpCertExpiryKESPeriod)
                    , (currentKESPeriod,          els ! ElCurrentKESPeriod)
                    , (remainingKESPeriods,       els ! ElRemainingKESPeriods)
                    , (remainingKESPeriodsInDays, els ! ElRemainingKESPeriodsInDays)
                    ]

updateNodeVersion
  :: Text
  -> Bool
  -> Element
  -> UI ()
updateNodeVersion _           False _  = return ()
updateNodeVersion nodeVersion True  el =
  if (strip nodeVersion) `elem` supportedNodesVersions
    then
      void $ element el #. []
                        # set text (unpack nodeVersion)
                        # set UI.title__ ""
    else do
      void $ element el #. [UnsupportedVersion]
                        # set text (unpack nodeVersion)
                        # set UI.title__ ("Unsupported node version, please use these versions only: "
                                          <> unpack showSupportedNodesVersions)

updateElement
  :: ElementValue
  -> Bool
  -> Element
  -> UI ()
updateElement _  False _  = return ()
updateElement ev True  el = do
  let textValue =
        case ev of
          ElementInt     i -> show i
          ElementInteger i -> show i
          ElementWord64  w -> show w
          ElementDouble  d -> showWith1DecPlace d
          ElementString  s -> s
          ElementText    t -> unpack t
  void $ element el # set text textValue

updateProgressBar
  :: Double
  -> Element
  -> UI ()
updateProgressBar percents bar = void $
  element bar # set style [("width", showWith1DecPlace preparedPercents <> "%")]
 where
  -- Sometimes (for CPU usage) percents can be bigger than 100%,
  -- in this case actual width of bar should be 100%.
  preparedPercents = if percents > 100.0 then 100.0 else percents

showWith1DecPlace :: Double -> String
showWith1DecPlace = unpack . sformat ("" % fixed 1)

updateNodeCommit
  :: Text
  -> Text
  -> Bool
  -> Element
  -> UI ()
updateNodeCommit _      _           False _ = return ()
updateNodeCommit commit shortCommit True  commitHref = do
  sComm <- UI.string $ unpack shortCommit
  void $ element commitHref # set UI.href ("https://github.com/input-output-hk/cardano-node/commit/"
                                           <> unpack commit)
                            # set children [sComm]

updateEndpoint
  :: String
  -> Element
  -> UI ()
updateEndpoint endpoint endpointLabel = void $
  element endpointLabel # set text shortened
                        # set UI.title__ fullEndpointTitle
 where
  len = length endpoint
  shortened = if len > 20
                then take 10 endpoint <> "..." <> drop (len - 10) endpoint
                else endpoint
  fullEndpointTitle = if shortened == endpoint then "" else endpoint

updateSystemStart
  :: UTCTime
  -> Element
  -> UI ()
updateSystemStart systemStart systemStartLabel =
  void $ element systemStartLabel # set text systemStartFormatted
 where
  systemStartFormatted = formatTime defaultTimeLocale "%F %T %Z" systemStart

updateNodeUpTime
  :: UTCTime
  -> Element
  -> UI ()
updateNodeUpTime startTime upTimeLabel = do
  let nullDay = UTCTime (ModifiedJulianDay 0) 0
  upTimeDiff <-
    if startTime /= nullDay
      then do
        -- nodeStartTime received from the node.
        now <- liftIO $ getCurrentTime
        let upTimeDiff = now `diffUTCTime` startTime
        return upTimeDiff
      else
         -- No nodeStartTime were received (yet).
         return 0

  if upTimeDiff == 0
    then void $ element upTimeLabel # set text "00:00:00"
    else do
      let upTime = upTimeDiff `addUTCTime` nullDay
          upTimeFormatted = formatTime defaultTimeLocale "%X" upTime
          daysNum = utctDay upTime `diffDays` utctDay nullDay
          upTimeWithDays = if daysNum > 0
                             -- Show days only if upTime is bigger than 23:59:59.
                             then show daysNum <> "d " <> upTimeFormatted
                             else upTimeFormatted
      void $ element upTimeLabel # set text upTimeWithDays

-- | Since peers list will be changed dynamically, we need it
--   to update corresponding HTML-murkup dynamically as well.
--   Please note that we don't change DOM actully (to avoid possible space leak).
updatePeersList
  :: [PeerInfo]
  -> Bool
  -> [PeerInfoItem]
  -> UI ()
updatePeersList peersInfo' changed peersInfoItems = do
  -- The number of connected peers may reduce, so hide all items by default.
  mapM_ (hideElement . piItem) peersInfoItems

  let peersInfo =
        if length peersInfo' > length peersInfoItems
          then
            -- We prepared peer items for known number of connected peers,
            -- but the number of connected peers is bigger than prepared items.
            -- Show only first N items.
            take (length peersInfoItems) peersInfo'
          else
            peersInfo'
  -- Show N items, corresponding to the number of connected peers,
  -- and fill them with actual values.
  let peersInfoWithIndices = zip peersInfo [0 .. length peersInfo - 1]
  forM_ peersInfoWithIndices $ \(PeerInfo {..}, i) -> do
    let item  = peersInfoItems L.!! i
        PeerInfoElements {..} = piItemElems item
    -- Update internal elements of item using actual values.
    updateElement (ElementString piEndpoint)   changed pieEndpoint
    updateElement (ElementString piBytesInF)   changed pieBytesInF
    updateElement (ElementString piReqsInF)    changed pieReqsInF
    updateElement (ElementString piBlocksInF)  changed pieBlocksInF
    updateElement (ElementString piSlotNumber) changed pieSlotNumber
    updateElement (ElementString piStatus)     changed pieStatus
    -- Make item visible.
    showElement $ piItem item

updateKESInfo :: [(Integer, Element)] -> UI ()
updateKESInfo valuesWithElems =
  forM_ valuesWithElems $ \(value, kesElem) ->
    if value == 9999999999
      -- This value cannot be such a big, so it wasn't replaced by the
      -- real metric. It means there's no KES at all (node uses an old protocol).
      then updateElement (ElementString "—")    True kesElem
      else updateElement (ElementInteger value) True kesElem

updateErrorsList
  :: [NodeError]
  -> Bool
  -> Element
  -> UI ()
updateErrorsList _          False _ = return ()
updateErrorsList nodeErrors True  errorsList = do
  errors <- forM nodeErrors $ \(NodeError utcTimeStamp sev msg) -> do
    let aClass :: HTMLClass
        aClass = case sev of
                      Warning   -> WarningMessage
                      Error     -> ErrorMessage
                      Critical  -> CriticalMessage
                      Alert     -> AlertMessage
                      Emergency -> EmergencyMessage
                      _         -> NoClass
    let timeStamp = formatTime defaultTimeLocale "%F %T" utcTimeStamp

    UI.div #. [W3Row] #+
      [ UI.div #. [W3Third, W3Theme] #+ [UI.div #+ [UI.string timeStamp]]
      , UI.div #. [W3TwoThird, W3Theme] #+ [UI.div #. [aClass] #+ [UI.string msg]]
      ]
  void $ element errorsList # set children errors

updateErrorsTab
  :: [NodeError]
  -> Bool
  -> Element
  -> UI ()
updateErrorsTab _          False _ = return ()
updateErrorsTab nodeErrors True  errorsTab =
  if null nodeErrors
    then disableErrorsTab
    else enableErrorsTab
 where
   disableErrorsTab = void $ element errorsTab # set UI.enabled False
                                               # set UI.title__ "Good news: there are no errors!"
   enableErrorsTab  = void $ element errorsTab # set UI.enabled True
                                               # set UI.title__ "Errors"

mkTraceAcceptorEndpoint
  :: Text
  -> [RemoteAddrNamed]
  -> String
mkTraceAcceptorEndpoint nameOfNode acceptors =
  case maybeActiveNode of
    Just (RemoteAddrNamed _ (RemoteSocket host port)) -> host <> ":" <> port
    Just (RemoteAddrNamed _ (RemotePipe pipePath))    -> pipePath
    Nothing                                           -> "-"
 where
  maybeActiveNode = flip L.find acceptors $ \(RemoteAddrNamed name _) -> name == nameOfNode

showElement, hideElement :: Element -> UI Element
showElement w = element w # set UI.style [("display", "inline")]
hideElement w = element w # set UI.style [("display", "none")]

updateCharts
  :: UI.Window
  -> Text
  -> ResourcesMetrics
  -> NodeMetrics
  -> UI ()
updateCharts window nameOfNode rm nm = do
  now <- liftIO $ getCurrentTime
  let ts :: String
      ts = formatTime defaultTimeLocale "%M:%S" time
      time = timeDiff `addUTCTime` (UTCTime (ModifiedJulianDay 0) 0)
      timeDiff :: NominalDiffTime
      timeDiff = now `diffUTCTime` (nodeStartTime nm)

  mcId <- ifM (elementExists mN) (pure mN) (pure mGN)
  ccId <- ifM (elementExists cN) (pure cN) (pure cGN)
  dcId <- ifM (elementExists dN) (pure dN) (pure dGN)
  ncId <- ifM (elementExists nN) (pure nN) (pure nGN)

  UI.runFunction $ UI.ffi Chart.updateMemoryUsageChartJS  mcId ts (memory rm)
  UI.runFunction $ UI.ffi Chart.updateCPUUsageChartJS     ccId ts (cpuPercent rm)
  UI.runFunction $ UI.ffi Chart.updateDiskUsageChartJS    dcId ts (diskUsageR rm)     (diskUsageW rm)
  UI.runFunction $ UI.ffi Chart.updateNetworkUsageChartJS ncId ts (networkUsageIn rm) (networkUsageOut rm)
 where
  mN = showt MemoryUsageChartId  <> nameOfNode
  cN = showt CPUUsageChartId     <> nameOfNode
  dN = showt DiskUsageChartId    <> nameOfNode
  nN = showt NetworkUsageChartId <> nameOfNode

  mGN = showt GridMemoryUsageChartId  <> nameOfNode
  cGN = showt GridCPUUsageChartId     <> nameOfNode
  dGN = showt GridDiskUsageChartId    <> nameOfNode
  nGN = showt GridNetworkUsageChartId <> nameOfNode

  showt :: Show a => a -> Text
  showt = pack . show

  elementExists anId = isJust <$> UI.getElementById window (unpack anId)

  ifM :: Monad m => m Bool -> m a -> m a -> m a
  ifM b t f = do b' <- b; if b' then t else f

-- | If no metrics was received from the node for a long time
--   (more than 'active-node-life') this node is treated as idle.
--   Technically it means that the node is disconnected from RTView
--   or it wasn't connected to RTView at all.
checkIfNodeIsIdlePane
  :: RTViewParams
  -> Word64
  -> Element
  -> Element
  -> UI Bool
checkIfNodeIsIdlePane params metricsLastUpdate idleTag nodePane =
  checkIfNodeIsIdle params
                    metricsLastUpdate
                    idleTag
                    (void $ element nodePane # set UI.style [("opacity", "0.7")])
                    (void $ element nodePane # set UI.style [("opacity", "1.0")])

checkIfNodeIsIdleGrid
  :: UI.Window
  -> RTViewParams
  -> Word64
  -> Element
  -> Text
  -> UI Bool
checkIfNodeIsIdleGrid window params metricsLastUpdate idleTag nameOfNode =
  checkIfNodeIsIdle params
                    metricsLastUpdate
                    idleTag
                    (forNodeColumn $ set UI.style [("opacity", "0.7")])
                    (forNodeColumn $ set UI.style [("opacity", "1.0")])
 where
  forNodeColumn
    :: (UI Element -> UI Element)
    -> UI ()
  forNodeColumn action = do
    let cellsIdsForNodeColumn =
          map (\elemName -> show elemName <> "-" <> unpack nameOfNode)
              allMetricsNames
    let allCells = (show GridNodeTH <> unpack nameOfNode) : cellsIdsForNodeColumn
    forM_ allCells $ \anId ->
      UI.getElementById window anId >>= \case
        Just el -> void $ element el # action
        Nothing -> return ()

checkIfNodeIsIdle
  :: RTViewParams
  -> Word64
  -> Element
  -> UI ()
  -> UI ()
  -> UI Bool
checkIfNodeIsIdle RTViewParams {..}
                  metricsLastUpdate
                  idleTag
                  additionalActionOnIdle
                  additionalActionOnActive = do
  let lifetimeInNSec = secToNanosec rtvActiveNodeLife
  now <- liftIO getMonotonicTimeNSec
  if now - metricsLastUpdate > lifetimeInNSec
    then do
      markNodeAsIdle
      return True
    else do
      markNodeAsActive
      return False
 where
  secToNanosec :: Int -> Word64
  secToNanosec s = fromIntegral $ s * 1000000000
  markNodeAsIdle = do
    void $ showElement idleTag # set UI.title__ ("Node metrics have not been received for more than "
                                                 <> show rtvActiveNodeLife <> " seconds")
    additionalActionOnIdle
  markNodeAsActive = do
    void $ hideElement idleTag # set UI.title__ ""
    additionalActionOnActive
