{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.GUI.Updater
    ( updateGUI
    ) where

import           Cardano.Prelude hiding ((%))
import qualified Data.List as L
import           Data.Map.Strict ((!))
import           Data.Text (unpack)
import           Data.Time.Calendar (Day (..), diffDays)
import           Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Formatting (fixed, sformat, (%))
import           GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, children, element, set, style, text,
                                              ( # ), ( #+ ), ( #. ))
import           Prelude (String)

import           Cardano.RTView.CLI (RTViewParams (..))
import qualified Cardano.RTView.GUI.Charts as Chart
import           Cardano.RTView.GUI.Elements (ElementName (..), ElementValue (..),
                                              HTMLClass (..), HTMLId (..),
                                              HTMLW3Class (..), NodeStateElements,
                                              NodesStateElements,
                                              PeerInfoElements (..), PeerInfoItem (..),
                                              (<+>))
import           Cardano.RTView.NodeState.Types (NodeError (..), NodeInfo (..),
                                                 NodeMetrics (..), NodeState (..),
                                                 NodesState, PeerInfo (..))
import           Cardano.BM.Data.Configuration (RemoteAddr (..), RemoteAddrNamed (..))
import           Cardano.BM.Data.Severity (Severity (..))

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
  -- Only one GUI mode can be active now, so check it and update it.
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
updatePaneGUI window nodesState params acceptors nodesStateElems = do
  forM_ nodesStateElems $ \(nameOfNode, elements, peerInfoItems) -> do
    let nodeState = nodesState ! nameOfNode
        acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

    let ni = nsInfo nodeState
        nm = nsMetrics nodeState
        activeNodeMark = unpack nameOfNode

    updateCharts window nameOfNode ni nm

    void $ updateEndpoint     acceptorEndpoint                                $ elements ! ElTraceAcceptorEndpoint
    void $ updateElementValue (ElementString  $ niNodeRelease ni)             $ elements ! ElNodeRelease
    void $ updateElementValue (ElementString  $ niNodeVersion ni)             $ elements ! ElNodeVersion
    void $ updateElementValue (ElementString  $ niNodePlatform ni)            $ elements ! ElNodePlatform
    void $ updateNodeCommit   (niNodeCommit ni) (niNodeShortCommit ni)        $ elements ! ElNodeCommitHref
    void $ updateElementValue (ElementString activeNodeMark)                  $ elements ! ElActiveNode
    void $ updateNodeUpTime   (niUpTime ni)                                   $ elements ! ElUptime
    void $ updateElementValue (ElementInteger $ niEpoch ni)                   $ elements ! ElEpoch
    void $ updateElementValue (ElementInteger $ niSlot ni)                    $ elements ! ElSlot
    void $ updateElementValue (ElementInteger $ niBlocksNumber ni)            $ elements ! ElBlocksNumber
    void $ updateElementValue (ElementInteger $ niBlocksForgedNumber ni)      $ elements ! ElBlocksForgedNumber
    void $ updateElementValue (ElementInteger $ niNodeCannotForge ni)         $ elements ! ElNodeCannotForge
    void $ updateElementValue (ElementDouble  $ niChainDensity ni)            $ elements ! ElChainDensity
    void $ updateElementValue (ElementInteger $ niNodeIsLeaderNum ni)         $ elements ! ElNodeIsLeaderNumber
    void $ updateElementValue (ElementInteger $ niSlotsMissedNumber ni)       $ elements ! ElSlotsMissedNumber
    void $ updateElementValue (ElementInteger $ niTxsProcessed ni)            $ elements ! ElTxsProcessed
    void $ updateErrorsList   (niNodeErrors ni)                               $ elements ! ElNodeErrors
    void $ updateElementValue (ElementWord64  $ nmMempoolTxsNumber nm)        $ elements ! ElMempoolTxsNumber
    void $ updateElementValue (ElementDouble  $ nmMempoolTxsPercent nm)       $ elements ! ElMempoolTxsPercent
    void $ updateElementValue (ElementWord64  $ nmMempoolBytes nm)            $ elements ! ElMempoolBytes
    void $ updateElementValue (ElementDouble  $ nmMempoolBytesPercent nm)     $ elements ! ElMempoolBytesPercent
    void $ updateElementValue (ElementInteger $ nmMempoolMaxTxs nm)           $ elements ! ElMempoolMaxTxs
    void $ updateElementValue (ElementInteger $ nmMempoolMaxBytes nm)         $ elements ! ElMempoolMaxBytes
    void $ updateElementValue (ElementDouble  $ nmMemory nm)                  $ elements ! ElMemory
    void $ updateElementValue (ElementDouble  $ nmMemoryMax nm)               $ elements ! ElMemoryMax
    void $ updateElementValue (ElementDouble  $ nmMemoryMaxTotal nm)          $ elements ! ElMemoryMaxTotal
    void $ updateElementValue (ElementDouble  $ nmMemoryPercent nm)           $ elements ! ElMemoryPercent
    void $ updateElementValue (ElementDouble  $ nmCPUPercent nm)              $ elements ! ElCPUPercent
    void $ updateElementValue (ElementDouble  $ nmDiskUsageR nm)              $ elements ! ElDiskUsageR
    void $ updateElementValue (ElementDouble  $ nmDiskUsageRMaxTotal nm)      $ elements ! ElDiskUsageRMaxTotal
    void $ updateElementValue (ElementDouble  $ nmDiskUsageW nm)              $ elements ! ElDiskUsageW
    void $ updateElementValue (ElementDouble  $ nmDiskUsageWMaxTotal nm)      $ elements ! ElDiskUsageWMaxTotal
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageIn nm)          $ elements ! ElNetworkUsageIn
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageInMaxTotal nm)  $ elements ! ElNetworkUsageInMaxTotal
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageOut nm)         $ elements ! ElNetworkUsageOut
    void $ updateElementValue (ElementDouble  $ nmNetworkUsageOutMaxTotal nm) $ elements ! ElNetworkUsageOutMaxTotal
    void $ updateElementValue (ElementDouble  $ nmRTSMemoryAllocated nm)      $ elements ! ElRTSMemoryAllocated
    void $ updateElementValue (ElementDouble  $ nmRTSMemoryUsed nm)           $ elements ! ElRTSMemoryUsed
    void $ updateElementValue (ElementDouble  $ nmRTSMemoryUsedPercent nm)    $ elements ! ElRTSMemoryUsedPercent
    void $ updateElementValue (ElementDouble  $ nmRTSGcCpu nm)                $ elements ! ElRTSGcCpu
    void $ updateElementValue (ElementDouble  $ nmRTSGcElapsed nm)            $ elements ! ElRTSGcElapsed
    void $ updateElementValue (ElementInteger $ nmRTSGcNum nm)                $ elements ! ElRTSGcNum
    void $ updateElementValue (ElementInteger $ nmRTSGcMajorNum nm)           $ elements ! ElRTSGcMajorNum

    updateKESInfo [ (niOpCertStartKESPeriod ni, elements ! ElOpCertStartKESPeriod)
                  , (niCurrentKESPeriod ni,     elements ! ElCurrentKESPeriod)
                  , (niRemainingKESPeriods ni,  elements ! ElRemainingKESPeriods)
                  ]

    updatePeersList (niPeersInfo ni) peerInfoItems

    void $ updateProgressBar (nmMempoolBytesPercent nm)    $ elements ! ElMempoolBytesProgress
    void $ updateProgressBar (nmMempoolTxsPercent nm)      $ elements ! ElMempoolTxsProgress
    void $ updateProgressBar (nmRTSMemoryUsedPercent nm)   $ elements ! ElRTSMemoryProgress

    markOutdatedElements params ni nm elements

updateGridGUI
  :: UI.Window
  -> NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> NodesStateElements
  -> UI ()
updateGridGUI window nodesState _params acceptors gridNodesStateElems =
  forM_ gridNodesStateElems $ \(nameOfNode, elements, _) -> do
    let nodeState = nodesState ! nameOfNode
        acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

    let ni = nsInfo nodeState
        nm = nsMetrics nodeState

    updateCharts window nameOfNode ni nm

    void $ updateEndpoint     acceptorEndpoint                           $ elements ! ElTraceAcceptorEndpoint
    void $ updateElementValue (ElementString  $ niNodeRelease ni)        $ elements ! ElNodeRelease
    void $ updateElementValue (ElementString  $ niNodeVersion ni)        $ elements ! ElNodeVersion
    void $ updateElementValue (ElementString  $ niNodePlatform ni)       $ elements ! ElNodePlatform
    void $ updateNodeCommit   (niNodeCommit ni) (niNodeShortCommit ni)   $ elements ! ElNodeCommitHref
    void $ updateElementValue (ElementInt     $ length (niPeersInfo ni)) $ elements ! ElPeersNumber
    void $ updateNodeUpTime   (niUpTime ni)                              $ elements ! ElUptime
    void $ updateElementValue (ElementInteger $ niEpoch ni)              $ elements ! ElEpoch
    void $ updateElementValue (ElementInteger $ niSlot ni)               $ elements ! ElSlot
    void $ updateElementValue (ElementInteger $ niBlocksNumber ni)       $ elements ! ElBlocksNumber
    void $ updateElementValue (ElementInteger $ niBlocksForgedNumber ni) $ elements ! ElBlocksForgedNumber
    void $ updateElementValue (ElementInteger $ niNodeCannotForge ni)    $ elements ! ElNodeCannotForge
    void $ updateElementValue (ElementDouble  $ niChainDensity ni)       $ elements ! ElChainDensity
    void $ updateElementValue (ElementInteger $ niNodeIsLeaderNum ni)    $ elements ! ElNodeIsLeaderNumber
    void $ updateElementValue (ElementInteger $ niSlotsMissedNumber ni)  $ elements ! ElSlotsMissedNumber
    void $ updateElementValue (ElementInteger $ niTxsProcessed ni)       $ elements ! ElTxsProcessed
    void $ updateElementValue (ElementWord64  $ nmMempoolTxsNumber nm)   $ elements ! ElMempoolTxsNumber
    void $ updateElementValue (ElementWord64  $ nmMempoolBytes nm)       $ elements ! ElMempoolBytes
    void $ updateElementValue (ElementDouble  $ nmRTSGcCpu nm)           $ elements ! ElRTSGcCpu
    void $ updateElementValue (ElementDouble  $ nmRTSGcElapsed nm)       $ elements ! ElRTSGcElapsed
    void $ updateElementValue (ElementInteger $ nmRTSGcNum nm)           $ elements ! ElRTSGcNum
    void $ updateElementValue (ElementInteger $ nmRTSGcMajorNum nm)      $ elements ! ElRTSGcMajorNum

    updateKESInfo [ (niOpCertStartKESPeriod ni, elements ! ElOpCertStartKESPeriod)
                  , (niCurrentKESPeriod ni,     elements ! ElCurrentKESPeriod)
                  , (niRemainingKESPeriods ni,  elements ! ElRemainingKESPeriods)
                  ]

updateElementValue
  :: ElementValue
  -> Element
  -> UI Element
updateElementValue (ElementInt     i) el = element el # set text (show i)
updateElementValue (ElementInteger i) el = element el # set text (show i)
updateElementValue (ElementWord64  w) el = element el # set text (show w)
updateElementValue (ElementDouble  d) el = element el # set text (showWith1DecPlace d)
updateElementValue (ElementString  s) el = element el # set text s

updateProgressBar
  :: Double
  -> Element
  -> UI Element
updateProgressBar percents bar = do
  element bar # set style [("width", showWith1DecPlace preparedPercents <> "%")]
 where
  -- Sometimes (for CPU usage) percents can be bigger than 100%,
  -- in this case actual width of bar should be 100%.
  preparedPercents = if percents > 100.0 then 100.0 else percents

showWith1DecPlace :: Double -> String
showWith1DecPlace = unpack . sformat ("" % fixed 1)

updateNodeCommit
  :: String
  -> String
  -> Element
  -> UI Element
updateNodeCommit commit shortCommit commitHref = do
  sComm <- UI.string shortCommit
  element commitHref # set UI.href ("https://github.com/input-output-hk/cardano-node/commit/" <> commit)
                     # set children [sComm]

updateEndpoint
  :: String
  -> Element
  -> UI Element
updateEndpoint endpoint endpointLabel =
  element endpointLabel # set text shortened
                        # set UI.title__ fullEndpointTitle
 where
  len = length endpoint
  shortened = if len > 20
                then take 10 endpoint <> "..." <> drop (len - 10) endpoint
                else endpoint
  fullEndpointTitle = if shortened == endpoint then "" else endpoint

updateNodeUpTime
  :: Word64
  -> Element
  -> UI Element
updateNodeUpTime upTimeInNs upTimeLabel =
  element upTimeLabel # set text upTimeWithDays
 where
  upTimeInSec :: Double
  upTimeInSec = fromIntegral upTimeInNs / 1000000000
  -- We show up time as time with seconds, so we don't need fractions of second.
  upTimeDiff :: NominalDiffTime
  upTimeDiff = fromInteger $ round upTimeInSec
  nullDay = UTCTime (ModifiedJulianDay 0) 0
  upTime = upTimeDiff `addUTCTime` nullDay
  upTimeFormatted = formatTime defaultTimeLocale "%X" upTime
  daysNum = (utctDay upTime) `diffDays` (utctDay nullDay)
  upTimeWithDays = if daysNum > 0
                     -- Show days only if upTime is bigger than 23:59:59.
                     then show daysNum <> "d " <> upTimeFormatted
                     else upTimeFormatted

-- | Since peers list will be changed dynamically, we need it
--   to update corresponding HTML-murkup dynamically as well.
--   Please note that we don't change DOM actully (to avoid possible space leak).
updatePeersList
  :: [PeerInfo]
  -> [PeerInfoItem]
  -> UI ()
updatePeersList peersInfo' peersInfoItems = do
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
  forM_ peersInfoWithIndices $ \(pI, i) -> do
    let item = peersInfoItems L.!! i
        internalElems = piItemElems item
    -- Update internal elements of item using actual values.
    void $ updateElementValue (ElementString (piEndpoint   pI)) $ pieEndpoint   internalElems
    void $ updateElementValue (ElementString (piBytesInF   pI)) $ pieBytesInF   internalElems
    void $ updateElementValue (ElementString (piReqsInF    pI)) $ pieReqsInF    internalElems
    void $ updateElementValue (ElementString (piBlocksInF  pI)) $ pieBlocksInF  internalElems
    void $ updateElementValue (ElementString (piSlotNumber pI)) $ pieSlotNumber internalElems
    void $ updateElementValue (ElementString (piStatus     pI)) $ pieStatus     internalElems
    -- Make item visible.
    showElement $ piItem item

updateKESInfo :: [(Integer, Element)] -> UI ()
updateKESInfo valuesWithElems =
  forM_ valuesWithElems $ \(value, kesElem) ->
    if value == 9999999999
      -- This value cannot be such a big, so it wasn't replaced by the
      -- real metric. It means there's no KES at all (node uses an old protocol).
      then void $ updateElementValue (ElementString "—")    kesElem
      else void $ updateElementValue (ElementInteger value) kesElem

updateErrorsList
  :: [NodeError]
  -> Element
  -> UI Element
updateErrorsList nodeErrors errorsList = do
  errors <- forM nodeErrors $ \(NodeError utcTimeStamp sev msg) -> do
    let className :: String
        className = case sev of
                      Warning   -> show WarningMessage
                      Error     -> show ErrorMessage
                      Critical  -> show CriticalMessage
                      Alert     -> show AlertMessage
                      Emergency -> show EmergencyMessage
                      _         -> ""
    let timeStamp = formatTime defaultTimeLocale "%F %T" utcTimeStamp

    UI.div #. show W3Row #+
      [ UI.div #. [W3Third, W3Theme] <+> [] #+ [UI.div #+ [UI.string timeStamp]]
      , UI.div #. [W3TwoThird, W3Theme] <+> [] #+ [UI.div #. className #+ [UI.string msg]]
      ]
  element errorsList # set children errors

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

-- | If some metric wasn't receive for a long time -
--   we mark corresponding value in GUI as outdated one.
markOutdatedElements
  :: RTViewParams
  -> NodeInfo
  -> NodeMetrics
  -> NodeStateElements
  -> UI ()
markOutdatedElements params ni nm els = do
  now <- liftIO $ getMonotonicTimeNSec
  -- Different metrics have different lifetime.
  let niLife  = rtvNodeInfoLife params
      bcLife  = rtvBlockchainInfoLife params
      rtsLife = rtvRTSInfoLife params

  markValues now (niUpTimeLastUpdate ni) niLife [ els ! ElUptime
                                                , els ! ElNodeRelease
                                                , els ! ElNodeVersion
                                                , els ! ElNodePlatform
                                                , els ! ElNodeCommitHref
                                                ]
  markValues now (niEpochLastUpdate ni) bcLife [els ! ElEpoch]
  markValues now (niOpCertStartKESPeriodLastUpdate ni) niLife [els ! ElOpCertStartKESPeriod]
  markValues now (niCurrentKESPeriodLastUpdate ni)     niLife [els ! ElCurrentKESPeriod]
  markValues now (niRemainingKESPeriodsLastUpdate ni)  niLife [els ! ElRemainingKESPeriods]

  markValues now (niSlotLastUpdate ni)               bcLife [els ! ElSlot]
  markValues now (niBlocksNumberLastUpdate ni)       bcLife [els ! ElBlocksNumber]
  markValues now (niBlocksForgedNumberLastUpdate ni) bcLife [ els ! ElBlocksForgedNumber
                                                            , els ! ElNodeCannotForge
                                                            ]
  markValues now (niChainDensityLastUpdate ni)       bcLife [els ! ElChainDensity]
  markValues now (niSlotsMissedNumberLastUpdate ni)  bcLife [els ! ElSlotsMissedNumber]
  markValues now (niNodeIsLeaderNumLastUpdate ni)    bcLife [els ! ElNodeIsLeaderNumber]

  markValues now (nmRTSGcCpuLastUpdate nm)      rtsLife [els ! ElRTSGcCpu]
  markValues now (nmRTSGcElapsedLastUpdate nm)  rtsLife [els ! ElRTSGcElapsed]
  markValues now (nmRTSGcNumLastUpdate nm)      rtsLife [els ! ElRTSGcNum]
  markValues now (nmRTSGcMajorNumLastUpdate nm) rtsLife [els ! ElRTSGcMajorNum]

  -- Mark progress bars' state.
  markProgressBar now (nmRTSMemoryLastUpdate nm) rtsLife els ( ElRTSMemoryProgress
                                                             , ElRTSMemoryProgressBox
                                                             )
                                                             [ ElRTSMemoryAllocated
                                                             , ElRTSMemoryUsed
                                                             , ElRTSMemoryUsedPercent
                                                             ]

markValues
  :: Word64
  -> Word64
  -> Word64
  -> [Element]
  -> UI ()
markValues now lastUpdate lifetime els =
  if now - lastUpdate > lifetime
    then mapM_ (void . markAsOutdated) els
    else mapM_ (void . markAsUpToDate) els

showElement, hideElement :: Element -> UI Element
showElement w = element w # set UI.style [("display", "inline")]
hideElement w = element w # set UI.style [("display", "none")]

markAsOutdated, markAsUpToDate :: Element -> UI Element
markAsOutdated el = element el # set UI.class_ (show OutdatedValue)
markAsUpToDate el = element el # set UI.class_ ""

markProgressBar
  :: Word64
  -> Word64
  -> Word64
  -> NodeStateElements
  -> (ElementName, ElementName)
  -> [ElementName]
  -> UI ()
markProgressBar now lastUpdate lifetime els (barName, barBoxName) labelsNames =
  if now - lastUpdate > lifetime
    then markBarAsOutdated
    else markBarAsUpToDate
 where
  bar    = els ! barName
  barBox = els ! barBoxName

  markBarAsOutdated = do
    void $ element bar    # set UI.class_ (show ProgressBarOutdated)
    void $ element barBox # set UI.class_ (show ProgressBarBoxOutdated)
                          # set UI.title__ "The progress values are outdated"
    forM_ labelsNames $ \name -> void . markAsOutdated $ els ! name
  markBarAsUpToDate = do
    void $ element bar    # set UI.class_ (show ProgressBar)
    void $ element barBox # set UI.class_ (show ProgressBarBox)
                          # set UI.title__ ""
    forM_ labelsNames $ \name -> void . markAsUpToDate $ els ! name

updateCharts
  :: UI.Window
  -> Text
  -> NodeInfo
  -> NodeMetrics
  -> UI ()
updateCharts window nameOfNode ni nm = do
  mcId <- ifM (elementExists mN) (pure mN) (pure mGN)
  ccId <- ifM (elementExists cN) (pure cN) (pure cGN)
  dcId <- ifM (elementExists dN) (pure dN) (pure dGN)
  ncId <- ifM (elementExists nN) (pure nN) (pure nGN)

  UI.runFunction $ UI.ffi Chart.updateMemoryUsageChartJS  mcId ts (nmMemory nm)
  UI.runFunction $ UI.ffi Chart.updateCPUUsageChartJS     ccId ts (nmCPUPercent nm)
  UI.runFunction $ UI.ffi Chart.updateDiskUsageChartJS    dcId ts (nmDiskUsageR nm)     (nmDiskUsageW nm)
  UI.runFunction $ UI.ffi Chart.updateNetworkUsageChartJS ncId ts (nmNetworkUsageIn nm) (nmNetworkUsageOut nm)
 where
  ts :: String
  ts = formatTime defaultTimeLocale "%M:%S" time
  time = addUTCTime timeDiff (UTCTime (ModifiedJulianDay 0) 0)
  timeDiff :: NominalDiffTime
  timeDiff = fromInteger $ round timeInSec
  timeInSec :: Double
  timeInSec = fromIntegral (niUpTime ni) / 1000000000

  mN = show MemoryUsageChartId  <> nameOfNode
  cN = show CPUUsageChartId     <> nameOfNode
  dN = show DiskUsageChartId    <> nameOfNode
  nN = show NetworkUsageChartId <> nameOfNode

  mGN = show GridMemoryUsageChartId  <> nameOfNode
  cGN = show GridCPUUsageChartId     <> nameOfNode
  dGN = show GridDiskUsageChartId    <> nameOfNode
  nGN = show GridNetworkUsageChartId <> nameOfNode

  elementExists anId = isJust <$> UI.getElementById window (unpack anId)
