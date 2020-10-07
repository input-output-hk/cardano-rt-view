{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.GUI.Pane
    ( mkNodePane
    ) where

import           Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Prelude (String)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, set, string, ( # ), ( #+ ),
                                              ( #. ))

import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), HTMLW3Class (..),
                                              NodeStateElements, PeerInfoElements (..),
                                              PeerInfoItem (..), hideIt, showIt,
                                              ( ## ), (<+>))

mkNodePane
  :: Text
  -> UI (Element, NodeStateElements, [PeerInfoItem])
mkNodePane nameOfNode = do
  -- Create |Element|s containing node state (info, metrics).
  -- These elements will be part of the complete page,
  -- later they will be updated by acceptor thread.
  elNodeProtocol            <- string ""
  elNodeVersion             <- string ""
  elNodePlatform            <- string ""
  elActiveNode              <- string "-"
  elUptime                  <- string "00:00:00"
  elEpoch                   <- string "0"
  elSlot                    <- string "0"
  elBlocksNumber            <- string "0"
  elBlocksForgedNumber      <- string "0"
  elNodeCannotForge         <- string "0"
  elChainDensity            <- string "0"
  elNodeIsLeaderNumber      <- string "0"
  elSlotsMissedNumber       <- string "0"
  elTxsProcessed            <- string "0"
  elTraceAcceptorEndpoint   <- string "0"
  elOpCertStartKESPeriod    <- string "0"
  elCurrentKESPeriod        <- string "0"
  elRemainingKESPeriods     <- string "0"
  elMempoolTxsNumber        <- string "0"
  elMempoolTxsPercent       <- string "0"
  elMempoolBytes            <- string "0"
  elMempoolBytesPercent     <- string "0"
  elMempoolMaxTxs           <- string "0"
  elMempoolMaxBytes         <- string "0"
  elMemory                  <- string "0"
  elMemoryMax               <- string "0"
  elMemoryMaxTotal          <- string "0"
  elMemoryPercent           <- string "0"
  elCPUPercent              <- string "0"
  elDiskUsageR              <- string "0"
  elDiskUsageRMaxTotal      <- string "0"
  elDiskUsageW              <- string "0"
  elDiskUsageWMaxTotal      <- string "0"
  elNetworkUsageIn          <- string "0"
  elNetworkUsageInMaxTotal  <- string "0"
  elNetworkUsageOut         <- string "0"
  elNetworkUsageOutMaxTotal <- string "0"
  elRTSMemoryAllocated      <- string "0"
  elRTSMemoryUsed           <- string "0"
  elRTSMemoryUsedPercent    <- string "0"
  elRTSGcCpu                <- string "0"
  elRTSGcElapsed            <- string "0"
  elRTSGcNum                <- string "0"
  elRTSGcMajorNum           <- string "0"

  -- Progress bars.
  elMempoolBytesProgress    <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elMempoolBytes
                                 , UI.span #. show PercentsSlashHSpacer #+ [string "/"]
                                 , element elMempoolBytesPercent
                                 , string "%"
                                 ]
  elMempoolBytesProgressBox <- UI.div #. show ProgressBarBox #+ [element elMempoolBytesProgress]

  elMempoolTxsProgress      <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elMempoolTxsNumber
                                 , UI.span #. show PercentsSlashHSpacer #+ [string "/"]
                                 , element elMempoolTxsPercent
                                 , string "%"
                                 ]
  elMempoolTxsProgressBox   <- UI.div #. show ProgressBarBox #+ [element elMempoolTxsProgress]

  elRTSMemoryProgress       <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elRTSMemoryUsed
                                 , UI.span #. show BarValueUnit #+ [string "MB"]
                                 ]
  elRTSMemoryProgressBox    <- UI.div #. show ProgressBarBox #+ [element elRTSMemoryProgress]

  elNodeCommitHref <- UI.anchor # set UI.href ""
                                # set UI.target "_blank"
                                # set UI.title__ "Browse cardano-node repository on this commit"
                                #+ [string ""]

  -- Create content area for each tab.
  nodeTabContent
    <- UI.div #. show TabContainer # showIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Half #+
                 [ UI.div #+ [string "Node protocol"]
                 , UI.div #+ [string "Node version"]
                 , UI.div #+ [string "Node platform"]
                 , UI.div #+ [string "Node commit"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "Node uptime"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "Node endpoint"]
                 , vSpacer NodeInfoVSpacer
                 ]
             , UI.div #. show W3Half #+
                 [ UI.div #. show NodeInfoValues #+
                     [ UI.span #. show ReleaseName #+ [element elNodeProtocol]
                     , UI.div #+ [element elNodeVersion]
                     , UI.div #+ [element elNodePlatform]
                     , UI.div #. show CommitLink #+ [element elNodeCommitHref]
                     , vSpacer NodeInfoVSpacer
                     , UI.div #+ [element elUptime]
                     , vSpacer NodeInfoVSpacer
                     , UI.div #+ [element elTraceAcceptorEndpoint]
                     , vSpacer NodeInfoVSpacer
                     ]
                 ]
             ]
         ]

  kesTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Half #+
                 [ UI.div #+ [string "Start KES period"]
                 , UI.div #+ [string "KES period"]
                 , UI.div #+ [string "KES remaining"]
                 , vSpacer NodeInfoVSpacer
                 ]
             , UI.div #. show W3Half #+
                 [ UI.div #. show NodeInfoValues #+
                     [ UI.div #+ [element elOpCertStartKESPeriod]
                     , UI.div #+ [element elCurrentKESPeriod]
                     , UI.div #+ [element elRemainingKESPeriods]
                     , vSpacer NodeInfoVSpacer
                     ]
                 ]
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

         peerItem <- UI.div #. show W3Row # set UI.style [("display", "none")] #+
                       [ UI.div #. [W3Quarter] <+> [NodeMetricsValues] #+
                           [element endpoint]
                       , UI.div #. [W3Quarter, W3RightAlign] <+> [NodeMetricsValues] #+
                           [element slotNumber]
                       , UI.div #. [W3Quarter, W3RightAlign] <+> [NodeMetricsValues] #+
                           [ element bytesInF
                           , string " / "
                           , element reqsInF
                           , string " / "
                           , element blocksInF
                           ]
                       , UI.div #. [W3Quarter, W3RightAlign] <+> [NodeMetricsValues] #+
                           [element status]
                       ]
         return ( element peerItem
                , PeerInfoItem
                    peerItem
                    (PeerInfoElements endpoint bytesInF reqsInF blocksInF slotNumber status)
                )
  let (elPeersList, peerInfoItems) = unzip peersList

  peersTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Quarter #+
                 [ string "Endpoint"
                 ]
             , UI.div #. [W3Quarter, W3RightAlign] <+> [] #+
                 [ string "Slot No."
                 ]
             , UI.div #. [W3Quarter, W3RightAlign] <+> [] #+
                 [ string "In Flight: b/r/bl" # set UI.title__ "In Flight: bytes/reqs/blocks"
                 ]
             , UI.div #. [W3Quarter, W3RightAlign] <+> [] #+
                 [ string "Status"
                 ]
             ]
         , UI.div #+ elPeersList
         ]

  blockchainTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Half #+
                 [ UI.div #+ [string "Epoch / Slot in epoch"]
                 , UI.div #+ [string "Blocks number"]
                 , UI.div #+ [string "Forged blocks number"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "Chain density"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "Slot leader, number"]
                 , UI.div #+ [string "Cannot forge, number"]
                 , UI.div #+ [string "Missed slots number"]
                 , vSpacer NodeInfoVSpacer
                 ]
             , UI.div #. show W3Half #+
                 [ UI.div #. show NodeInfoValues #+
                     [ UI.div #+
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
                         , UI.span #. show DensityPercent #+ [string "%"]
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
             ]
         ]

  mempoolTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.div #. show W3Row #+
                 [ UI.div #. show W3Half #+ [string "Mempool | bytes"]
                 , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                     [ element elMempoolMaxBytes
                     , infoMark "Maximum in bytes"
                     ]
                 ]
                 , element elMempoolBytesProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. show W3Container #+
             [ UI.div #. show W3Row #+
                 [ UI.div #. show W3Half #+ [string "Mempool | TXs"]
                 , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                     [ element elMempoolMaxTxs
                     , infoMark "Maximum in txs"
                     ]
                 ]
                 , element elMempoolTxsProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. show W3Row #+
              [ UI.div #. show W3Theme #+
                  [ string "TXs processed"
                  , nbsp
                  , nbsp
                  , nbsp
                  , UI.span #. show NodeInfoValues #+ [element elTxsProcessed]
                  ]
              ]
         , vSpacer NodeMetricsVSpacer
         ]

  resourcesTabMemoryContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.canvas ## (show MemoryUsageChartId <> T.unpack nameOfNode)
                         #. show MemoryUsageChart
                         #+ []
             ]
         ]

  resourcesTabCPUContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.canvas ## (show CPUUsageChartId <> T.unpack nameOfNode)
                         #. show CPUUsageChart
                         #+ []
             ]
         ]

  resourcesTabDiskContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.canvas ## (show DiskUsageChartId <> T.unpack nameOfNode)
                         #. show DiskUsageChart
                         #+ []
             ]
         ]

  resourcesTabNetworkContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.canvas ## (show NetworkUsageChartId <> T.unpack nameOfNode)
                         #. show NetworkUsageChart
                         #+ []
             ]
         ]

  ghcRTSTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.div #. show W3Row #+
                 [ UI.div #. show W3Half #+ [string "RTS live memory"]
                 , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                     [ element elRTSMemoryAllocated
                     , UI.span #. show ValueUnit #+ [string "MB"]
                     ]
                 ]
             , element elRTSMemoryProgressBox
             ]
         , vSpacer NodeMetricsVSpacer
         , UI.div #. show W3Row #+
             [ UI.div #. show W3Half #+
                 [ UI.div #+ [string "GC CPU time"]
                 , UI.div #+ [string "GC time elapsed"]
                 , UI.div #+ [string "Number of GC runs"]
                 , UI.div #+ [string "Major GC runs"]
                 ]
             , UI.div #. show W3Half #+
                 [ UI.div #. show NodeInfoValues #+
                     [ UI.div #+
                         [ element elRTSGcCpu
                         , UI.span #. show ValueUnit #+ [string "s"]
                         ]
                     , UI.div #+
                         [ element elRTSGcElapsed
                         , UI.span #. show ValueUnit #+ [string "s"]
                         ]
                     , UI.div #+ [element elRTSGcNum]
                     , UI.div #+ [element elRTSGcMajorNum]
                     ]
                 ]
             ]
         , vSpacer NodeMetricsVSpacer
         ]

  -- List of node errors, it will be changed dynamically!
  elNodeErrorsList <- UI.div #+ []

  errorsTabContent
    <- UI.div #. [] <+> [TabContainer, ErrorsTabContainer] # hideIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Third #+
                 [ string "Timestamp"
                 , infoMark "Time in UTC"
                 ]
             , UI.div #. show W3TwoThird #+
                 [ string "Error message"
                 ]
             ]
         , element elNodeErrorsList
         ]

  -- Tabs for corresponding sections.
  let tabButton title iconName =
        UI.button #. [W3BarItem, W3Button, W3Mobile] <+> []
                  # set UI.title__ title
                  #+ [UI.img #. show NodeMenuIcon
                             # set UI.src ("/static/images/" <> iconName)]
      anchorButton title iconName =
        UI.anchor #. [W3BarItem, W3Button, W3Mobile] <+> []
                  # set UI.href "#"
                  #+ [ UI.img #. show ResourcesIcon
                              # set UI.src ("/static/images/" <> iconName)
                     , string title
                     ]

  nodeTab       <- tabButton "Node info" "info.svg" # makeItActive
  kesTab        <- tabButton "Key Evolving Signature" "key.svg"
  peersTab      <- tabButton "Peers" "peers.svg"
  blockchainTab <- tabButton "Blockchain" "blockchain.svg"
  mempoolTab    <- tabButton "Mempool" "mempool.svg"
  ghcRTSTab     <- tabButton "RTS GC" "rts.svg"
  errorsTab     <- tabButton "Errors" "bugs.svg"

  resourcesTabMemory  <- anchorButton "Memory" "memory.svg"
  resourcesTabCPU     <- anchorButton "CPU" "cpu.svg"
  resourcesTabDisk    <- anchorButton "Disk" "disk.svg"
  resourcesTabNetwork <- anchorButton "Network" "network.svg"

  resourcesTab  <- UI.div #. [W3DropdownHover, W3Mobile] <+> [] #+
                     [ UI.button #. show W3Button
                                 # set UI.title__ "Resources"
                                 #+
                         [ UI.img #. show NodeMenuIcon # set UI.src "/static/images/resources.svg"
                         , string " ▾"
                         ]
                     , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] <+> [] #+
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
  nodeWidget <-
    UI.div #. [W3Container, W3Margin, W3Border] <+> [NodeContainer] #+
      [ UI.div #. show NodeNameArea #+
          [ string "Name: "
          , UI.span #. show NodeName #+ [ element elActiveNode ]
          ]
      , UI.div #. [W3Bar] <+> [NodeBar] #+
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
  let nodeStateElems =
        Map.fromList
          [ (ElNodeProtocol,            elNodeProtocol)
          , (ElNodeVersion,             elNodeVersion)
          , (ElNodePlatform,            elNodePlatform)
          , (ElNodeCommitHref,          elNodeCommitHref)
          , (ElActiveNode,              elActiveNode)
          , (ElUptime,                  elUptime)
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
          , (ElCurrentKESPeriod,        elCurrentKESPeriod)
          , (ElRemainingKESPeriods,     elRemainingKESPeriods)
          , (ElNodeErrors,              elNodeErrorsList)
          , (ElNodeErrorsTab,           errorsTab)
          , (ElMempoolTxsNumber,        elMempoolTxsNumber)
          , (ElMempoolTxsPercent,       elMempoolTxsPercent)
          , (ElMempoolBytes,            elMempoolBytes)
          , (ElMempoolBytesPercent,     elMempoolBytesPercent)
          , (ElMempoolMaxTxs,           elMempoolMaxTxs)
          , (ElMempoolMaxBytes,         elMempoolMaxBytes)
          , (ElMemory,                  elMemory)
          , (ElMemoryMax,               elMemoryMax)
          , (ElMemoryMaxTotal,          elMemoryMaxTotal)
          , (ElMemoryPercent,           elMemoryPercent)
          , (ElCPUPercent,              elCPUPercent)
          , (ElDiskUsageR,              elDiskUsageR)
          , (ElDiskUsageRMaxTotal,      elDiskUsageRMaxTotal)
          , (ElDiskUsageW,              elDiskUsageW)
          , (ElDiskUsageWMaxTotal,      elDiskUsageWMaxTotal)
          , (ElNetworkUsageIn,          elNetworkUsageIn)
          , (ElNetworkUsageInMaxTotal,  elNetworkUsageInMaxTotal)
          , (ElNetworkUsageOut,         elNetworkUsageOut)
          , (ElNetworkUsageOutMaxTotal, elNetworkUsageOutMaxTotal)
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

  return (nodeWidget, nodeStateElems, peerInfoItems)

---

vSpacer :: HTMLClass -> UI Element
vSpacer className = UI.div #. show className #+ []

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
makeItActive   = set UI.class_ ([W3BarItem, W3Button, W3Mobile] <+> [ActiveTab])
makeItInactive = set UI.class_ ([W3BarItem, W3Button, W3Mobile] <+> [])

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. show InfoMark
          #  set UI.title__ aTitle
          #+ [ UI.img #. show InfoMarkImg
                      # set UI.src "/static/images/question.svg" #+ []
             ]

nbsp :: UI Element
nbsp = UI.span # set UI.html "&nbsp;" #+ []
