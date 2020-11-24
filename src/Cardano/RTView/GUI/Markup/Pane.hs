  {-# LANGUAGE LambdaCase #-}
  {-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.GUI.Markup.Pane
    ( mkNodesPanes
    ) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVarIO)
import           Control.Monad (forM, forM_, void)
import           Control.Monad.STM (atomically)
import           Data.HashMap.Strict ((!), (!?))
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, (#), (#+))

import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), NodeStateElements, NodesStateElements,
                                              PeerInfoElements (..), PeerInfoItem (..), dataAttr,
                                              hideIt, showIt, (##), (#.))
import           Cardano.RTView.GUI.Updater (justUpdateErrorsListAndTab)
import           Cardano.RTView.NodeState.Types

mkNodesPanes
  :: TVar NodesState
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements, [(Text, Element)])
mkNodesPanes nsTVar acceptors = do
  nodesState <- liftIO $ readTVarIO nsTVar

  nodePanesWithElems
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         -- Explicitly set flags for peers list and errors list: in this case these lists
         -- will be visible even after reload/reopen of the web-page.
         setChangedFlag nameOfNode (\ns -> ns { nodeErrors   = (nodeErrors ns)   { errorsChanged    = True } })
         setChangedFlag nameOfNode (\ns -> ns { peersMetrics = (peersMetrics ns) { peersInfoChanged = True } })

         (pane, nodeStateElems, peerInfoItems) <-
           mkNodePane nsTVar (nodesState ! nameOfNode) nameOfNode acceptors
         return (nameOfNode, pane, nodeStateElems, peerInfoItems)

  panesAreas
    <- forM nodePanesWithElems $ \(_, pane, _, _) ->
         return $ UI.div #. [W3Col, W3L6, W3M12, W3S12] #+ [element pane]

  let nodesEls       = [(name, elems, piItems) | (name, _,    elems, piItems) <- nodePanesWithElems]
      panesWithNames = [(name, pane)           | (name, pane, _,     _)       <- nodePanesWithElems]

  panes <- UI.div #. [W3Row] #+ panesAreas

  return (panes, nodesEls, panesWithNames)
 where
  setChangedFlag :: Text -> (NodeState -> NodeState) -> UI ()
  setChangedFlag nameOfNode mkNewNS =
    liftIO . atomically $ modifyTVar' nsTVar $ \currentNS ->
      case currentNS !? nameOfNode of
        Just ns -> HM.adjust (const $ mkNewNS ns) nameOfNode currentNS
        Nothing -> currentNS

mkNodePane
  :: TVar NodesState
  -> NodeState
  -> Text
  -> [RemoteAddrNamed]
  -> UI (Element, NodeStateElements, [PeerInfoItem])
mkNodePane nsTVar NodeState {..} nameOfNode acceptors = do
  let MempoolMetrics {..}    = mempoolMetrics
      ForgeMetrics {..}      = forgeMetrics
      RTSMetrics {..}        = rtsMetrics
      BlockchainMetrics {..} = blockchainMetrics
      KESMetrics {..}        = kesMetrics
      NodeMetrics {..}       = nodeMetrics

  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elIdleNode <- string "Idle" #. [IdleNode] # hideIt

  let acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

  elNodeProtocol              <- string $ showText nodeProtocol
  elNodeVersion               <- string $ showText nodeVersion
  elNodePlatform              <- string $ showText nodePlatform
  elUptime                    <- string   showInitTime
  elSystemStartTime           <- string $ showTime systemStartTime
  elEpoch                     <- string $ showInteger epoch
  elSlot                      <- string $ showInteger slot
  elBlocksNumber              <- string $ showInteger blocksNumber
  elBlocksForgedNumber        <- string $ showInteger blocksForgedNumber
  elNodeCannotForge           <- string $ showInteger nodeCannotForge
  elChainDensity              <- string $ showDouble  chainDensity
  elNodeIsLeaderNumber        <- string $ showInteger nodeIsLeaderNum
  elSlotsMissedNumber         <- string $ showInteger slotsMissedNumber
  elTxsProcessed              <- string $ showInteger txsProcessed
  elTraceAcceptorEndpoint     <- string   acceptorEndpoint
                                        # set UI.title__ (fullEndpointTitle acceptorEndpoint)
  elOpCertStartKESPeriod      <- string $ showInteger opCertStartKESPeriod
  elOpCertExpiryKESPeriod     <- string $ showInteger opCertExpiryKESPeriod
  elCurrentKESPeriod          <- string $ showInteger currentKESPeriod
  elRemainingKESPeriods       <- string $ showInteger remKESPeriods
  elRemainingKESPeriodsInDays <- string $ showInteger remKESPeriodsInDays
  elMempoolTxsNumber          <- string $ showInteger mempoolTxsNumber
  elMempoolTxsPercent         <- string $ showDouble  mempoolTxsPercent
  elMempoolBytes              <- string $ showWord64  mempoolBytes
  elMempoolBytesPercent       <- string $ showDouble  mempoolBytesPercent
  elMempoolMaxTxs             <- string $ showInteger mempoolMaxTxs
  elMempoolMaxBytes           <- string $ showInteger mempoolMaxBytes
  elRTSMemoryAllocated        <- string $ showDouble  rtsMemoryAllocated
  elRTSMemoryUsed             <- string $ showDouble  rtsMemoryUsed
  elRTSMemoryUsedPercent      <- string $ showDouble  rtsMemoryUsedPercent
  elRTSGcCpu                  <- string $ showDouble  rtsGcCpu
  elRTSGcElapsed              <- string $ showDouble  rtsGcElapsed
  elRTSGcNum                  <- string $ showInteger rtsGcNum
  elRTSGcMajorNum             <- string $ showInteger rtsGcMajorNum

  -- Progress bars.
  elMempoolBytesProgress    <- UI.div #. [ProgressBar] #+
                                 [ UI.span #. [HSpacer] #+ []
                                 , element elMempoolBytes
                                 , string "/" #. [PercentsSlashHSpacer]
                                 , element elMempoolBytesPercent
                                 , string "%"
                                 ]
  elMempoolBytesProgressBox <- UI.div #. [ProgressBarBox] #+ [element elMempoolBytesProgress]

  elMempoolTxsProgress      <- UI.div #. [ProgressBar] #+
                                 [ UI.span #. [HSpacer] #+ []
                                 , element elMempoolTxsNumber
                                 , string "/" #. [PercentsSlashHSpacer]
                                 , element elMempoolTxsPercent
                                 , string "%"
                                 ]
  elMempoolTxsProgressBox   <- UI.div #. [ProgressBarBox] #+ [element elMempoolTxsProgress]

  elRTSMemoryProgress       <- UI.div #. [ProgressBar] #+
                                 [ UI.span #. [HSpacer] #+ []
                                 , element elRTSMemoryUsed
                                 , string "MB" #. [BarValueUnit]
                                 ]
  elRTSMemoryProgressBox    <- UI.div #. [ProgressBarBox] #+ [element elRTSMemoryProgress]

  elNodeCommitHref <- UI.anchor # set UI.href ""
                                # set UI.target "_blank"
                                # set UI.title__ "Browse cardano-node repository on this commit"
                                # set UI.text (showText nodeShortCommit)

  -- Create content area for each tab.
  nodeTabContent
    <- UI.div #. [TabContainer, W3Row] # showIt #+
         [ UI.div #. [W3Half] #+
             [ UI.div #+ [string "Node protocol" # set UI.title__ "Node's protocol"]
             , UI.div #+ [string "Node version"  # set UI.title__ "Version of the node"]
             , UI.div #+ [string "Node platform" # set UI.title__ "Platform the node is working on"]
             , UI.div #+ [string "Node commit"   # set UI.title__ "Git commit the node was built from"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Node uptime"   # set UI.title__ "How long the node is working"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Node endpoint" # set UI.title__ "Socket/pipe used to connect the node with RTView"]
             , vSpacer NodeInfoVSpacer
             ]
         , UI.div #. [W3Half, NodeInfoValues] #+
             [ UI.div #+ [element elNodeProtocol]
             , UI.div #+ [element elNodeVersion]
             , UI.div #+ [element elNodePlatform]
             , UI.div #. [CommitLink] #+ [element elNodeCommitHref]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [element elUptime]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [element elTraceAcceptorEndpoint]
             , vSpacer NodeInfoVSpacer
             ]
         ]

  kesTabContent
    <- UI.div #. [TabContainer, W3Row] # hideIt #+
         [ UI.div #. [W3Half] #+
             [ UI.div #+ [string "Start KES period"    # set UI.title__ "Certificate KES start period"]
             , UI.div #+ [string "Expiry KES period"   # set UI.title__ "Certificate KES expiry period"]
             , UI.div #+ [string "KES period"          # set UI.title__ "Current KES period"]
             , UI.div #+ [string "KES remaining"       # set UI.title__ "KES periods until expiry"]
             , UI.div #+ [string "KES remaining, days" # set UI.title__ "KES periods until expiry, in days"]
             , vSpacer NodeInfoVSpacer
             ]
         , UI.div #. [W3Half, NodeInfoValues] #+
             [ UI.div #+ [element elOpCertStartKESPeriod]
             , UI.div #+ [element elOpCertExpiryKESPeriod]
             , UI.div #+ [element elCurrentKESPeriod]
             , UI.div #+ [element elRemainingKESPeriods]
             , UI.div #+ [element elRemainingKESPeriodsInDays]
             , vSpacer NodeInfoVSpacer
             ]
         ]

  -- List of items corresponding to each peer. To avoid dynamic changes of DOM
  -- (unfortunately, it can be a reason of space leak), we create 20 (hidden) rows
  -- corresponding to 20 connected peers. Theoretically, the number of connected
  -- peers can be bigger, but the policy of ouroboros-network is about 20 hot peers
  -- (or less).
  let supportedPeersNum = 20 :: Int -- TODO: Probably cardano-node cab trace this number?
  peersList :: [(UI Element, PeerInfoItem)]
    <- forM [1..supportedPeersNum] $ const $ do
         endpoint   <- string ""
         slotNumber <- string ""
         bytesInF   <- string ""
         reqsInF    <- string ""
         blocksInF  <- string ""
         status     <- string ""

         peerItem
           <- UI.div #. [W3Row] # set UI.style [("display", "none")] #+
                [ UI.div #. [W3Quarter, NodeMetricsValues] #+
                    [element endpoint]
                , UI.div #. [W3Quarter, W3RightAlign, NodeMetricsValues] #+
                    [element slotNumber]
                , UI.div #. [W3Quarter, W3RightAlign, NodeMetricsValues] #+
                    [ element bytesInF # set UI.title__ "Sum of the byte count of blocks expected from all in-flight fetch requests"
                    , string " / "
                    , element reqsInF # set UI.title__ "Number of blocks fetch requests that are currently in-flight"
                    , string " / "
                    , element blocksInF # set UI.title__ "Blocks that are currently in-flight"
                    ]
                , UI.div #. [W3Quarter, W3RightAlign, NodeMetricsValues] #+
                    [element status]
                ]
         return ( element peerItem
                , PeerInfoItem
                    peerItem
                    (PeerInfoElements endpoint bytesInF reqsInF blocksInF slotNumber status)
                )
  let (elPeersList, peerInfoItems) = unzip peersList

  peersTabContent
    <- UI.div #. [TabContainer, W3Row] # hideIt #+
         [ UI.div #. [W3Quarter] #+
             [ string "Endpoint" # set UI.title__ "How the peer connected to this node"
             ]
         , UI.div #. [W3Quarter, W3RightAlign] #+
             [ string "Slot No." # set UI.title__ "Total number of peers reported by peer"
             ]
         , UI.div #. [W3Quarter, W3RightAlign] #+
             [ string "In Flight: b/r/bl" # set UI.title__ "In Flight: bytes/requests/blocks"
             ]
         , UI.div #. [W3Quarter, W3RightAlign] #+
             [ string "Status" # set UI.title__ "Peer's status"
             ]
         , UI.div #+ elPeersList
         ]

  blockchainTabContent
    <- UI.div #. [TabContainer, W3Row] # hideIt #+
         [ UI.div #. [W3Half] #+
             [ UI.div #+ [string "Blockchain start time"
                          # set UI.title__ "The time when this blockchain has started"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Epoch / Slot in epoch"
                          # set UI.title__ "Number of current epoch / number of the current slot in this epoch"]
             , UI.div #+ [string "Blocks number"
                          # set UI.title__ "Total number of blocks in this blockchain"]
             , UI.div #+ [string "Forged blocks number"
                          # set UI.title__ "Number of blocks forged by this node"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Chain density"
                          # set UI.title__ "Chain density, in percents"]
             , vSpacer NodeInfoVSpacer
             , UI.div #+ [string "Slot leader, number"
                          # set UI.title__ "Number of slots when this node was a leader"]
             , UI.div #+ [string "Cannot forge, number"
                          # set UI.title__ "Number of slots when this node was a leader but because of misconfiguration, it's impossible to forge a new block"]
             , UI.div #+ [string "Missed slots number"
                          # set UI.title__ "Number of slots when this node was a leader but didn't forge a new block"]
             , vSpacer NodeInfoVSpacer
             ]
         , UI.div #. [W3Half, NodeInfoValues] #+
             [ UI.div #+
                 [element elSystemStartTime]
             , vSpacer NodeInfoVSpacer
             , UI.div #+
                 [ element elEpoch
                 , string " / "
                 , element elSlot
                 ]
             , UI.div #+
                 [element elBlocksNumber]
             , UI.div #+
                 [element elBlocksForgedNumber]
             , vSpacer NodeInfoVSpacer
             , UI.div #+
                 [ element elChainDensity
                 , string "%" #. [DensityPercent]
                 ]
             , vSpacer NodeInfoVSpacer
             , UI.div #+
                 [element elNodeIsLeaderNumber]
             , UI.div #+
                 [element elNodeCannotForge]
             , UI.div #+
                 [element elSlotsMissedNumber]
             , vSpacer NodeInfoVSpacer
             ]
         ]

  mempoolTabContent
    <- UI.div #. [TabContainer] # hideIt #+
         [ UI.div #. [W3Container] #+
             [ UI.div #. [W3Row] #+
                 [ UI.div #. [W3Half] #+
                     [string "Mempool | bytes"
                      # set UI.title__ "Size of all transactions in the mempool, in bytes"]
                 , UI.div #. [W3Half, W3RightAlign] #+
                     [ element elMempoolMaxBytes
                     , infoMark "Maximum in bytes"
                     ]
                 ]
                 , element elMempoolBytesProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. [W3Container] #+
             [ UI.div #. [W3Row] #+
                 [ UI.div #. [W3Half] #+
                     [string "Mempool | TXs"
                      # set UI.title__ "Number of transactions in the mempool"]
                 , UI.div #. [W3Half, W3RightAlign] #+
                     [ element elMempoolMaxTxs
                     , infoMark "Maximum in txs"
                     ]
                 ]
                 , element elMempoolTxsProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. [W3Row] #+
              [ string "TXs processed"
                  # set UI.title__ "Number of processed transactions in this blockchain (these transactions are already removed from the mempool)"
              , element elTxsProcessed #. [TXsProcessed]
              ]
         , vSpacer NodeMetricsVSpacer
         ]

  resourcesTabMemoryContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show MemoryUsageChartId <> T.unpack nameOfNode)
                     #. [MemoryUsageChart]
                     #+ []
         ]

  resourcesTabCPUContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show CPUUsageChartId <> T.unpack nameOfNode)
                     #. [CPUUsageChart]
                     #+ []
         ]

  resourcesTabDiskContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show DiskUsageChartId <> T.unpack nameOfNode)
                     #. [DiskUsageChart]
                     #+ []
         ]

  resourcesTabNetworkContent
    <- UI.div #. [W3Container, TabContainer] # hideIt #+
         [ UI.canvas ## (show NetworkUsageChartId <> T.unpack nameOfNode)
                     #. [NetworkUsageChart]
                     #+ []
         ]

  ghcRTSTabContent
    <- UI.div #. [TabContainer] # hideIt #+
         [ UI.div #. [W3Container] #+
             [ UI.div #. [W3Row] #+
                 [ UI.div #. [W3Half] #+
                     [string "RTS live memory"
                        # set UI.title__ "Total amount of live data in the heap, in MB (updated after every GC)"]
                 , UI.div #. [W3Half, W3RightAlign] #+
                     [ element elRTSMemoryAllocated
                     , UI.span #. [ValueUnit] #+ [string "MB"]
                     ]
                 ]
             , element elRTSMemoryProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. [W3Row] #+
             [ UI.div #. [W3Half] #+
                 [ UI.div #+ [string "GC CPU time"       # set UI.title__ "Total CPU time used by the GC, in seconds"]
                 , UI.div #+ [string "GC time elapsed"   # set UI.title__ "Total elapsed time used by the GC, in seconds"]
                 , UI.div #+ [string "Number of GC runs" # set UI.title__ "Total number of GCs"]
                 , UI.div #+ [string "Major GC runs"     # set UI.title__ "Total number of major (oldest generation) GCs"]
                 ]
             , UI.div #. [W3Half, NodeInfoValues] #+
                 [ UI.div #+
                     [ element elRTSGcCpu
                     , string "s" #. [ValueUnit]
                     ]
                 , UI.div #+
                     [ element elRTSGcElapsed
                     , string "s" #. [ValueUnit]
                     ]
                 , UI.div #+ [element elRTSGcNum]
                 , UI.div #+ [element elRTSGcMajorNum]
                 ]
             ]
         , vSpacer NodeMetricsVSpacer
         ]

  -- List of node errors, it will be changed dynamically!
  elNodeErrorsList <- UI.div #. [ErrorsTabList] #+ []

  let dataSortByTime = "data-sortByTime"
      dataSortBySev  = "data-sortBySev"
      desc = "desc"
      asc  = "asc"

  elSortByTime  <- UI.img #. [ErrorsSortIcon]
                          # set UI.src "/static/images/sort.svg"
                          # set (dataAttr dataSortByTime) desc
                          # set UI.title__ "Sort by time, latest first"
  elSortBySev   <- UI.img #. [ErrorsSortIcon]
                          # set UI.src "/static/images/sort.svg"
                          # set (dataAttr dataSortBySev) desc
                          # set UI.title__ "Sort by severity level, worst first"
  elFilterBySev <- UI.img #. [ErrorsFilterIcon]
                          # set UI.src "/static/images/filter.svg"
                          # set UI.title__ "Filter by severity level"

  elRemoveAllErrors <- UI.img #. [ErrorsRemoveIcon]
                              # set UI.src "/static/images/trash.svg"
                              # set UI.title__ "Remove all messages"

  filterWarning   <- UI.anchor #. [W3BarItem, W3Button, W3Mobile] # set UI.href "#" #+
                       [ UI.string "W" #. [WarningMessageTagNoHelp]
                       , UI.string "Warning"
                       ]
  filterError     <- UI.anchor #. [W3BarItem, W3Button, W3Mobile] # set UI.href "#" #+
                       [ UI.string "E" #. [ErrorMessageTagNoHelp]
                       , UI.string "Error"
                       ]
  filterCritical  <- UI.anchor #. [W3BarItem, W3Button, W3Mobile] # set UI.href "#" #+
                       [ UI.string "C" #. [CriticalMessageTagNoHelp]
                       , UI.string "Critical"
                       ]
  filterAlert     <- UI.anchor #. [W3BarItem, W3Button, W3Mobile] # set UI.href "#" #+
                       [ UI.string "A" #. [AlertMessageTagNoHelp]
                       , UI.string "Alert"
                       ]
  filterEmergency <- UI.anchor #. [W3BarItem, W3Button, W3Mobile] # set UI.href "#" #+
                       [ UI.string "E" #. [EmergencyMessageTagNoHelp]
                       , UI.string "Emergency"
                       ]
  unFilter        <- UI.anchor #. [W3BarItem, W3Button, W3Mobile, W3BorderTop, W3Disabled]
                               # set UI.href "#"
                               #+ [ UI.string "Reset" ]

  let csvFile = "cardano-rt-view-" <> T.unpack nameOfNode <> "-errors.csv"
  errorsAsCSV <- mkCSVWithErrors nsTVar nameOfNode
  downloadErrorsAsCSV <- UI.anchor # set UI.href ("data:application/csv;charset=utf-8," <> errorsAsCSV)
                                   # set (UI.attr "download") csvFile
                                   #+ [ UI.img #. [ErrorsDownloadIcon]
                                               # set UI.src "/static/images/file-download.svg"
                                               # set UI.title__ "Download errors as CSV"
                                      ]

  errorsTabContent
    <- UI.div #. [TabContainer] # hideIt #+
         [ UI.div #. [W3Row, ErrorsTabHeader] #+
             [ UI.div #. [W3Third] #+
                 [ string "Timestamp"
                 , element elSortByTime
                 ]
             , UI.div #. [W3TwoThird] #+
                 [ UI.div #. [W3Row] #+
                     [ UI.div #. [W3TwoThird] #+
                         [ string "Message"
                         , element elSortBySev
                         , UI.div #. [W3DropdownHover, W3Mobile] #+
                             [ element elFilterBySev
                             , UI.img #. [ErrorsFilterDropdownIcon] # set UI.src "/static/images/dropdown-dark.svg"
                             , UI.div #. [W3DropdownContent, W3BarBlock] #+
                                 [ element filterWarning
                                 , element filterError
                                 , element filterCritical
                                 , element filterAlert
                                 , element filterEmergency
                                 , element unFilter
                                 ]
                             ]
                         , element downloadErrorsAsCSV
                         ]
                     , UI.div #. [W3Third, W3RightAlign] #+
                         [ element elRemoveAllErrors
                         ]
                     ]
                 ]
             ]
         , element elNodeErrorsList
         ]

  -- Tabs for corresponding sections.
  let tabButton :: String -> String -> Maybe Element -> UI Element
      tabButton title iconName maybeBadge = do
        let buttonContent =
              case maybeBadge of
                Just badge ->
                  [ UI.img #. [NodeMenuIcon] # set UI.src ("/static/images/" <> iconName)
                  , element badge
                  ]
                Nothing ->
                  [ UI.img #. [NodeMenuIcon] # set UI.src ("/static/images/" <> iconName)
                  ]
        UI.button #. [W3BarItem, W3Button, W3Mobile]
                  # set UI.title__ title
                  #+ buttonContent

      anchorButton title iconName =
        UI.anchor #. [W3BarItem, W3Button, W3Mobile]
                  # set UI.href "#"
                  #+ [ UI.img #. [ResourcesIcon]
                              # set UI.src ("/static/images/" <> iconName)
                     , string title
                     ]

  nodeTab       <- tabButton "Node info" "info.svg" Nothing # makeItActive
  kesTab        <- tabButton "Key Evolving Signature" "key.svg" Nothing
  peersTab      <- tabButton "Peers" "peers.svg" Nothing
  blockchainTab <- tabButton "Blockchain" "blockchain.svg" Nothing
  mempoolTab    <- tabButton "Mempool" "mempool.svg" Nothing
  ghcRTSTab     <- tabButton "RTS GC" "rts.svg" Nothing
  errorsBadge   <- UI.span #. [W3Badge, ErrorsBadge] # hideIt #+ [string ""]
  errorsTab     <- tabButton "Errors" "bugs.svg" (Just errorsBadge) # set UI.enabled False

  void $ UI.onEvent (UI.click elSortByTime) $ \_ -> do
    UI.get (dataAttr dataSortByTime) elSortByTime >>= \case
      "desc" -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsByTimeDesc
        void $ element elSortByTime # set (dataAttr dataSortByTime) asc
                                    # set UI.title__ "Sort by time, earlier first"
      _ -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsByTimeAsc
        void $ element elSortByTime # set (dataAttr dataSortByTime) desc
                                    # set UI.title__ "Sort by time, latest first"
    immediatelyUpdateErrors nsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  void $ UI.onEvent (UI.click elSortBySev) $ \_ -> do
    UI.get (dataAttr dataSortBySev) elSortBySev >>= \case
      "desc" -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsBySevDesc
        void $ element elSortBySev # set (dataAttr dataSortBySev) asc
                                   # set UI.title__ "Sort by severity level, warnings first"
      _ -> do
        setErrorsViewMode nsTVar nameOfNode $ sortErrors sortErrorsBySevAsc
        void $ element elSortBySev # set (dataAttr dataSortBySev) desc
                                   # set UI.title__ "Sort by severity level, worst first"
    immediatelyUpdateErrors nsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  let makeResetButtonActive   = void $ element unFilter #. [W3BarItem, W3Button, W3Mobile, W3BorderTop]
      makeResetButtonInactive = void $ element unFilter #. [W3BarItem, W3Button, W3Mobile, W3BorderTop, W3Disabled]

  let registerClickOnFilter :: Element -> Severity -> UI ()
      registerClickOnFilter el sev =
        void $ UI.onEvent (UI.click el) $ \_ -> do
          setErrorsViewMode nsTVar nameOfNode $ filterErrors sev
          makeResetButtonActive
          immediatelyUpdateErrors nsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  registerClickOnFilter filterWarning   Warning
  registerClickOnFilter filterError     Error
  registerClickOnFilter filterCritical  Critical
  registerClickOnFilter filterAlert     Alert
  registerClickOnFilter filterEmergency Emergency

  let filterItemsThatCanBeAcive =
        zip [1 :: Int ..]
            [ filterWarning
            , filterError
            , filterCritical
            , filterAlert
            , filterEmergency
            ]

  void $ UI.onEvent (UI.click unFilter) $ \_ -> do
    setErrorsViewMode nsTVar nameOfNode unFilterErrors
    immediatelyUpdateErrors nsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge
    makeResetButtonInactive
    forM_ filterItemsThatCanBeAcive $ \(_, el) -> void $ element el # makeItInactive

  void $ UI.onEvent (UI.click elRemoveAllErrors) $ \_ -> do
    setErrorsViewMode nsTVar nameOfNode removeAllErrors
    immediatelyUpdateErrors nsTVar nameOfNode elNodeErrorsList errorsTab errorsBadge

  forM_ filterItemsThatCanBeAcive $ \(ix, el) ->
    void $ UI.onEvent (UI.click el) $ \_ ->
      forM_ filterItemsThatCanBeAcive $ \(ix', el') ->
        if ix == ix'
          then void $ element el' # makeItActive
          else void $ element el' # makeItInactive

  resourcesTabMemory  <- anchorButton "Memory" "memory.svg"
  resourcesTabCPU     <- anchorButton "CPU" "cpu.svg"
  resourcesTabDisk    <- anchorButton "Disk" "disk.svg"
  resourcesTabNetwork <- anchorButton "Network" "network.svg"

  resourcesTab  <- UI.div #. [W3DropdownHover, W3Mobile] #+
                     [ UI.button #. [W3Button]
                                 # set UI.title__ "Resources"
                                 #+
                         [ UI.img #. [NodeMenuIcon] # set UI.src "/static/images/resources.svg"
                         , UI.img #. [ResourcesDropdownIcon] # set UI.src "/static/images/dropdown-blue.svg"
                         ]
                     , UI.div #. [W3DropdownContent, W3BarBlock] #+
                         [ element resourcesTabMemory
                         , element resourcesTabCPU
                         , element resourcesTabDisk
                         , element resourcesTabNetwork
                         ]
                     ]

  let tabs :: [((Element, Element), Int)]
      tabs =
        let allTabs = [ (nodeTab,             nodeTabContent)
                      , (kesTab,              kesTabContent)
                      , (peersTab,            peersTabContent)
                      , (blockchainTab,       blockchainTabContent)
                      , (mempoolTab,          mempoolTabContent)
                      , (resourcesTabMemory,  resourcesTabMemoryContent)
                      , (resourcesTabCPU,     resourcesTabCPUContent)
                      , (resourcesTabDisk,    resourcesTabDiskContent)
                      , (resourcesTabNetwork, resourcesTabNetworkContent)
                      , (errorsTab,           errorsTabContent)
                      , (ghcRTSTab,           ghcRTSTabContent)
                      ]
        in zip allTabs [1..length allTabs]

  registerClicksOnTabs tabs

  -- Make a widget for one node.
  nodePane <-
    UI.div #. [W3Container, W3Margin, W3Border, NodeContainer] #+
      [ UI.div #. [NodeNameArea] #+
          [ string "Name: "
          , string (T.unpack nameOfNode) #. [NodeName]
          , element elIdleNode
          ]
      , UI.div #. [W3Bar, NodeBar] #+
          [ element nodeTab
          , element kesTab
          , element peersTab
          , element blockchainTab
          , element mempoolTab
          , element resourcesTab
          , element errorsTab
          , element ghcRTSTab
          ]
      , element nodeTabContent
      , element kesTabContent
      , element peersTabContent
      , element blockchainTabContent
      , element mempoolTabContent
      , element resourcesTabMemoryContent
      , element resourcesTabCPUContent
      , element resourcesTabDiskContent
      , element resourcesTabNetworkContent
      , element errorsTabContent
      , element ghcRTSTabContent
      ]

  -- Return these elements, they will be updated by another thread later.
  let nodeStateElems = HM.fromList
        [ (ElNodePane,                nodePane)
        , (ElIdleNode,                elIdleNode)
        , (ElNodeProtocol,            elNodeProtocol)
        , (ElNodeVersion,             elNodeVersion)
        , (ElNodePlatform,            elNodePlatform)
        , (ElNodeCommitHref,          elNodeCommitHref)
        , (ElUptime,                  elUptime)
        , (ElSystemStartTime,         elSystemStartTime)
        , (ElEpoch,                   elEpoch)
        , (ElSlot,                    elSlot)
        , (ElBlocksNumber,            elBlocksNumber)
        , (ElBlocksForgedNumber,      elBlocksForgedNumber)
        , (ElNodeCannotForge,         elNodeCannotForge)
        , (ElChainDensity,            elChainDensity)
        , (ElNodeIsLeaderNumber,      elNodeIsLeaderNumber)
        , (ElSlotsMissedNumber,       elSlotsMissedNumber)
        , (ElTxsProcessed,            elTxsProcessed)
        , (ElTraceAcceptorEndpoint,   elTraceAcceptorEndpoint)
        , (ElOpCertStartKESPeriod,    elOpCertStartKESPeriod)
        , (ElOpCertExpiryKESPeriod,   elOpCertExpiryKESPeriod)
        , (ElCurrentKESPeriod,        elCurrentKESPeriod)
        , (ElRemainingKESPeriods,     elRemainingKESPeriods)
        , (ElRemainingKESPeriodsInDays, elRemainingKESPeriodsInDays)
        , (ElNodeErrors,              elNodeErrorsList)
        , (ElNodeErrorsTab,           errorsTab)
        , (ElNodeErrorsTabBadge,      errorsBadge)
        , (ElMempoolTxsNumber,        elMempoolTxsNumber)
        , (ElMempoolTxsPercent,       elMempoolTxsPercent)
        , (ElMempoolBytes,            elMempoolBytes)
        , (ElMempoolBytesPercent,     elMempoolBytesPercent)
        , (ElMempoolMaxTxs,           elMempoolMaxTxs)
        , (ElMempoolMaxBytes,         elMempoolMaxBytes)
        , (ElRTSMemoryAllocated,      elRTSMemoryAllocated)
        , (ElRTSMemoryUsed,           elRTSMemoryUsed)
        , (ElRTSMemoryUsedPercent,    elRTSMemoryUsedPercent)
        , (ElRTSGcCpu,                elRTSGcCpu)
        , (ElRTSGcElapsed,            elRTSGcElapsed)
        , (ElRTSGcNum,                elRTSGcNum)
        , (ElRTSGcMajorNum,           elRTSGcMajorNum)
        -- Progress bars
        , (ElMempoolBytesProgress,    elMempoolBytesProgress)
        , (ElMempoolBytesProgressBox, elMempoolBytesProgressBox)
        , (ElMempoolTxsProgress,      elMempoolTxsProgress)
        , (ElMempoolTxsProgressBox,   elMempoolTxsProgressBox)
        , (ElRTSMemoryProgress,       elRTSMemoryProgress)
        , (ElRTSMemoryProgressBox,    elRTSMemoryProgressBox)
        ]

  return (nodePane, nodeStateElems, peerInfoItems)

---

setErrorsViewMode
  :: TVar NodesState
  -> Text
  -> (NodeState -> NodeState)
  -> UI ()
setErrorsViewMode nsTVar nameOfNode mkNewNS =
  liftIO . atomically $ modifyTVar' nsTVar $ \currentNS ->
    case currentNS !? nameOfNode of
      Just ns -> HM.adjust (const $ mkNewNS ns) nameOfNode currentNS
      Nothing -> currentNS

sortErrors
  :: (NodeError -> NodeError -> Ordering)
  -> NodeState
  -> NodeState
sortErrors orderF ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = sortBy orderF currentErrors
    , errorsChanged = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics

filterErrors
  :: Severity
  -> NodeState
  -> NodeState
filterErrors targetSev ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = map showOnlyThisSeverity currentErrors
    , errorsChanged = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  showOnlyThisSeverity (NodeError ts sev msg _) = NodeError ts sev msg visible
   where
    visible = targetSev == sev

unFilterErrors
  :: NodeState
  -> NodeState
unFilterErrors ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = map makeVisible currentErrors
    , errorsChanged = True
    }
  currentMetrics = nodeErrors ns
  currentErrors = errors currentMetrics
  makeVisible (NodeError ts sev msg _) = NodeError ts sev msg True

removeAllErrors
  :: NodeState
  -> NodeState
removeAllErrors ns = ns { nodeErrors = newMetrics }
 where
  newMetrics = currentMetrics
    { errors = []
    , errorsChanged = True
    }
  currentMetrics = nodeErrors ns

immediatelyUpdateErrors
  :: TVar NodesState
  -> Text
  -> Element
  -> Element
  -> Element
  -> UI ()
immediatelyUpdateErrors nsTVar nameOfNode el elTab elTabBadge = do
  updatedState <- liftIO $ readTVarIO nsTVar
  let NodeState {..} = updatedState ! nameOfNode
  justUpdateErrorsListAndTab (errors nodeErrors) el elTab elTabBadge

mkCSVWithErrors
  :: TVar NodesState
  -> Text
  -> UI String
mkCSVWithErrors nsTVar nameOfNode = do
  currentState <- liftIO $ readTVarIO nsTVar
  let NodeState {..} = currentState ! nameOfNode
  return $ mkCSVWithErrorsForHref (errors nodeErrors)

vSpacer :: HTMLClass -> UI Element
vSpacer className = UI.div #. [className] #+ []

-- | Since information and metrics are splitted to tabs,
--   we have to make them clickable and show which one is active.
registerClicksOnTabs
  :: [((Element, Element), Int)]
  -> UI ()
registerClicksOnTabs tabs =
  forM_ tabs $ \((tab, _), tabNum) ->
    void $ UI.onEvent (UI.click tab) $ \_ -> showTabAndMakeItActive tabNum
 where
  showTabAndMakeItActive num =
    forM_ tabs $ \((tab', tabContent), tabNum') ->
      if num == tabNum'
        then do
          void $ element tabContent # showIt
          void $ element tab' # makeItActive
        else do
          void $ element tabContent # hideIt
          void $ element tab' # makeItInactive

makeItActive, makeItInactive :: UI Element -> UI Element
makeItActive el   = el #. [W3BarItem, W3Button, W3Mobile, ActiveTab]
makeItInactive el = el #. [W3BarItem, W3Button, W3Mobile]

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. [InfoMark]
          #  set UI.title__ aTitle
          #+ [ UI.img #. [InfoMarkImg]
                      # set UI.src "/static/images/question.svg" #+ []
             ]
