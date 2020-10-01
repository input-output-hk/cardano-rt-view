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
  elNodeRelease             <- string ""
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

  elMemoryProgress          <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elMemory
                                 , UI.span #. show BarValueUnit #+ [string "MB"]
                                 , UI.span #. show PercentsSlashHSpacer #+ [string "/"]
                                 , UI.span #. show PercentsSlashHRSpacer #+ [string "max"]
                                 , element elMemoryMax
                                 , UI.span #. show BarValueUnit #+ [string "MB"]
                                 ]
  elMemoryProgressBox       <- UI.div #. show ProgressBarBox #+ [element elMemoryProgress]

  elCPUProgress             <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elCPUPercent
                                 , string "%"
                                 ]
  elCPUProgressBox          <- UI.div #. show ProgressBarBox #+ [element elCPUProgress]

  elDiskUsageRProgress      <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elDiskUsageR
                                 , UI.span #. show BarValueUnit #+ [string "KB/s"]
                                 ]
  elDiskUsageRProgressBox   <- UI.div #. show ProgressBarBox #+ [element elDiskUsageRProgress]

  elDiskUsageWProgress      <- UI.div #. show ProgressBar #+
                                 [ UI.span #. show HSpacer #+ []
                                 , element elDiskUsageW
                                 , UI.span #. show BarValueUnit #+ [string "KB/s"]
                                 ]
  elDiskUsageWProgressBox   <- UI.div #. show ProgressBarBox #+ [element elDiskUsageWProgress]

  elNetworkUsageInProgress     <- UI.div #. show ProgressBar #+
                                    [ UI.span #. show HSpacer #+ []
                                    , element elNetworkUsageIn
                                    , UI.span #. show BarValueUnit #+ [string "KB/s"]
                                    ]
  elNetworkUsageInProgressBox  <- UI.div #. show ProgressBarBox #+ [element elNetworkUsageInProgress]

  elNetworkUsageOutProgress    <- UI.div #. show ProgressBar #+
                                    [ UI.span #. show HSpacer #+ []
                                    , element elNetworkUsageOut
                                    , UI.span #. show BarValueUnit #+ [string "KB/s"]
                                    ]
  elNetworkUsageOutProgressBox <- UI.div #. show ProgressBarBox #+ [element elNetworkUsageOutProgress]

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

  elNodeReleaseOutdateWarning    <- infoMark "The value is outdated"
  elNodeVersionOutdateWarning    <- infoMark "The value is outdated"
  elNodePlatformOutdateWarning   <- infoMark "The value is outdated"
  elNodeCommitHrefOutdateWarning <- infoMark "The value is outdated"
  elUptimeOutdateWarning         <- infoMark "The value is outdated"

  elOpCertStartKESPeriodOutdateWarning <- infoMark "The value is outdated"
  elCurrentKESPeriodOutdateWarning     <- infoMark "The value is outdated"
  elRemainingKESPeriodsOutdateWarning  <- infoMark "The value is outdated"

  elSlotOutdateWarning               <- infoMark "The value is outdated"
  elBlocksNumberOutdateWarning       <- infoMark "The value is outdated"
  elBlocksForgedNumberOutdateWarning <- infoMark "The value is outdated"
  elChainDensityOutdateWarning       <- infoMark "The value is outdated"
  elNodeIsLeaderNumberOutdateWarning <- infoMark "The value is outdated"
  elSlotsMissedNumberOutdateWarning  <- infoMark "The value is outdated"
  elForksCreatedNumberOutdateWarning <- infoMark "The value is outdated"

  elRTSGcCpuOutdateWarning       <- infoMark "The value is outdated"
  elRTSGcElapsedOutdateWarning   <- infoMark "The value is outdated"
  elRTSGcNumOutdateWarning       <- infoMark "The value is outdated"
  elRTSGcMajorNumOutdateWarning  <- infoMark "The value is outdated"

  -- Create content area for each tab.
  nodeTabContent
    <- UI.div #. show TabContainer # showIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Third #+
                 [ UI.div #+ [string "Node protocol:"]
                 , UI.div #+ [string "Node version:"]
                 , UI.div #+ [string "Node platform:"]
                 , UI.div #+ [string "Node commit:"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "Node uptime:"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "Start KES period:"]
                 , UI.div #+ [string "KES period:"]
                 , UI.div #+ [string "KES remaining:"]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [string "TraceAcceptor endpoint:"]
                 ]
             , UI.div #. show W3Third #+
                 [ UI.div #. show NodeInfoValues #+
                     [ UI.span #. show ReleaseName #+ [element elNodeRelease]
                     , UI.div #+ [element elNodeVersion]
                     , UI.div #+ [element elNodePlatform]
                     , UI.div #. show CommitLink #+ [element elNodeCommitHref]
                     , vSpacer NodeInfoVSpacer
                     , UI.div #+ [element elUptime]
                     , vSpacer NodeInfoVSpacer
                     , UI.div #+ [element elOpCertStartKESPeriod]
                     , UI.div #+ [element elCurrentKESPeriod]
                     , UI.div #+ [element elRemainingKESPeriods]
                     , vSpacer NodeInfoVSpacer
                     , UI.div #+ [element elTraceAcceptorEndpoint]
                     ]
                 ]
             , UI.div #. show W3Third #+
                 [ UI.span #+ [element elNodeReleaseOutdateWarning,         nbsp]
                 , UI.div #+ [element elNodeVersionOutdateWarning,          nbsp]
                 , UI.div #+ [element elNodePlatformOutdateWarning,         nbsp]
                 , UI.div #+ [element elNodeCommitHrefOutdateWarning,       nbsp]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [element elUptimeOutdateWarning,               nbsp]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [element elOpCertStartKESPeriodOutdateWarning, nbsp]
                 , UI.div #+ [element elCurrentKESPeriodOutdateWarning,     nbsp]
                 , UI.div #+ [element elRemainingKESPeriodsOutdateWarning,  nbsp]
                 , vSpacer NodeInfoVSpacer
                 , UI.div #+ [nbsp]
                 , UI.div #+ [nbsp]
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
                       [ UI.div #. show W3Col # set UI.style [("width", "32%")] #+ [ element endpoint ]
                       , UI.div #. show W3Col # set UI.style [("width", "12%")] #+ [ element slotNumber ]
                       , UI.div #. show W3Col # set UI.style [("width", "12%")] #+ [ element bytesInF ]
                       , UI.div #. show W3Col # set UI.style [("width", "12%")] #+ [ element reqsInF ]
                       , UI.div #. show W3Col # set UI.style [("width", "12%")] #+ [ element blocksInF ]
                       , UI.div #. show W3Col # set UI.style [("width", "20%")] #+ [ element status ]
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
             [ UI.div #. show W3Col # set UI.style [("width", "44%")] #+
                 [ nbsp ]
             , UI.div #. show W3Rest #+
                 [ UI.div #. show InFlight #+ [string "In Flight"]
                 ]
             ]
         , UI.div #. show W3Row #+
             [ UI.div #. show W3Col # set UI.style [("width", "32%")] #+
                 [ string "Endpoint"
                 ]
             , UI.div #. show W3Col # set UI.style [("width", "12%")] #+
                 [ string "Slot No."
                 ]
             , UI.div #. show W3Col # set UI.style [("width", "12%")] #+
                 [ UI.div #. show InFlightValues #+
                     [ string "Bytes"
                     ]
                 ]
             , UI.div #. show W3Col # set UI.style [("width", "12%")] #+
                 [ UI.div #. show InFlightValues #+
                     [ string "Reqs"
                     ]
                 ]
             , UI.div #. show W3Col # set UI.style [("width", "12%")] #+
                 [ UI.div #. show InFlightValues #+
                     [ string "Blocks"
                     ]
                 ]
             , UI.div #. show W3Rest #+
                 [ string "Status"
                 ]
             ]
         , UI.div #+ elPeersList
         ]

  blockchainTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Third #+
                 [ UI.div #+ [string "Epoch / Slot in epoch:"]
                 , UI.div #+ [string "Blocks number:"]
                 , UI.div #+ [string "Forged blocks number:"]
                 , UI.div #+ [string "Chain density:"]
                 , UI.div #+ [string "Slot leader, number:"]
                 , UI.div #+ [string "Cannot forge, number:"]
                 , UI.div #+ [string "Missed slots number:"]
                 ]
             , UI.div #. show W3Third #+
                 [ UI.div #. show NodeInfoValues #+
                     [ UI.div #+
                         [ element elEpoch
                         , string " / "
                         , element elSlot
                         ]
                     , UI.div #+ [element elBlocksNumber]
                     , UI.div #+ [element elBlocksForgedNumber]
                     , UI.div #+
                         [ element elChainDensity
                         , UI.span #. show DensityPercent #+ [string "%"]
                         ]
                     , UI.div #+ [element elNodeIsLeaderNumber]
                     , UI.div #+ [element elNodeCannotForge]
                     , UI.div #+ [element elSlotsMissedNumber]
                     ]
                 ]
             , UI.div #. show W3Third #+
                 [ UI.div #+ [element elSlotOutdateWarning,               nbsp]
                 , UI.div #+ [element elBlocksNumberOutdateWarning,       nbsp]
                 , UI.div #+ [element elBlocksForgedNumberOutdateWarning, nbsp]
                 , UI.div #+ [element elChainDensityOutdateWarning,       nbsp]
                 , UI.div #+ [element elNodeIsLeaderNumberOutdateWarning, nbsp]
                 , UI.div #+ [element elSlotsMissedNumberOutdateWarning,  nbsp]
                 , UI.div #+ [element elForksCreatedNumberOutdateWarning, nbsp]
                 ]
             ]
         ]

  mempoolTabContent
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Row #+
             [ twoElementsInRow
                (UI.div #. show W3Container #+
                   [ UI.div #. show W3Row #+
                       [ UI.div #. show W3Half #+ [string "Mempool | bytes"]
                       , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                           [ element elMempoolMaxBytes
                           , infoMark "Maximum in bytes"
                           ]
                       ]
                   , element elMempoolBytesProgressBox
                   ])
                (UI.div #. show W3Container #+
                   [ UI.div #. show W3Row #+
                       [ UI.div #. show W3Half #+ [string "Mempool | TXs"]
                       , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                           [ element elMempoolMaxTxs
                           , infoMark "Maximum in txs"
                           ]
                       ]
                   , element elMempoolTxsProgressBox
                   ])
             , vSpacer NodeMetricsVSpacer
             , UI.div #. show W3Row #+
                 [ UI.div #. show W3Theme #+
                     [ string "TXs processed:"
                     , nbsp
                     , nbsp
                     , UI.span #. show NodeInfoValues #+
                         [ element elTxsProcessed
                         ]
                     ]
                 ]
             ]
         ]

  resourcesTabContentCharts
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Container #+
             [ UI.div #. show W3Row #+
                 [ UI.div #. show W3Half #+
                     [ UI.canvas ## (show MemoryUsageChartId <> T.unpack nameOfNode)
                                 #. show MemoryUsageChart
                                 #+ []
                     ]
                 , UI.div #. show W3Half #+
                     [ UI.canvas ## (show CPUUsageChartId <> T.unpack nameOfNode)
                                 #. show CPUUsageChart
                                 #+ []
                     ]
                 ]
             , UI.div #. show W3Row #+
                 [ UI.div #. show W3Half #+
                     [ UI.canvas ## (show DiskUsageChartId <> T.unpack nameOfNode)
                                 #. show DiskUsageChart
                                 #+ []
                     ]
                 , UI.div #. show W3Half #+
                     [ UI.canvas ## (show NetworkUsageChartId <> T.unpack nameOfNode)
                                 #. show NetworkUsageChart
                                 #+ []
                     ]
                 ]
             ]
         ]

  resourcesTabContentBars
    <- UI.div #. show TabContainer # hideIt #+
         [ UI.div #. show W3Row #+
             [ UI.div #. show W3Container #+
                 [ UI.div #. show W3Row #+
                     [ UI.div #. show W3Half #+ [string "Memory usage"]
                     , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                         [ element elMemoryMaxTotal
                         , UI.span #. show ValueUnit #+ [string "MB"]
                         ]
                     ]
                 , element elMemoryProgressBox
                 ]
             , vSpacer NodeMetricsVSpacer
             , UI.div #. show W3Container #+
                 [ UI.div #. show W3Row #+
                     [ UI.div #. show W3Half #+ [string "CPU usage"]
                     , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                         [ string "100"
                         , UI.span #. show ValueUnitPercent #+ [string "%"]
                         ]
                     ]
                 , element elCPUProgressBox
                 ]
             , vSpacer NodeMetricsVSpacer
             , twoElementsInRow
                (UI.div #. show W3Container #+
                   [ UI.div #. show W3Row #+
                       [ UI.div #. show W3Half #+ [string "Disk | RD"]
                       , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                           [ element elDiskUsageRMaxTotal
                           , UI.span #. show ValueUnit #+ [string "KB/s"]
                           , infoMark "Maximum value over the last two minutes"
                           ]
                       ]
                   , element elDiskUsageRProgressBox
                   ])
                (UI.div #. show W3Container #+
                   [ UI.div #. show W3Row #+
                       [ UI.div #. show W3Half #+ [string "Disk | WR"]
                       , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                           [ element elDiskUsageWMaxTotal
                           , UI.span #. show ValueUnit #+ [string "KB/s"]
                           , infoMark "Maximum value over the last two minutes"
                           ]
                       ]
                   , element elDiskUsageWProgressBox
                   ])
             , vSpacer NodeMetricsVSpacer
             , twoElementsInRow
                (UI.div #. show W3Container #+
                   [ UI.div #. show W3Row #+
                       [ UI.div #. show W3Half #+ [string "Network | IN"]
                       , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                           [ element elNetworkUsageInMaxTotal
                           , UI.span #. show ValueUnit #+ [string "KB/s"]
                           ]
                       ]
                   , element elNetworkUsageInProgressBox
                   ])
                (UI.div #. show W3Container #+
                   [ UI.div #. show W3Row #+
                       [ UI.div #. show W3Half #+ [string "Network | OUT"]
                       , UI.div #. [W3Half, W3RightAlign] <+> [] #+
                           [ element elNetworkUsageOutMaxTotal
                           , UI.span #. show ValueUnit #+ [string "KB/s"]
                           ]
                       ]
                   , element elNetworkUsageOutProgressBox
                   ])
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
             [ UI.div #. show W3Third #+
                 [ UI.div #+ [string "GC CPU time:"]
                 , UI.div #+ [string "GC time elapsed:"]
                 , UI.div #+ [string "Number of GC runs:"]
                 , UI.div #+ [string "Major GC runs:"]
                 ]
             , UI.div #. show W3Third #+
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
             , UI.div #. show W3Third #+
                 [ UI.div #+ [element elRTSGcCpuOutdateWarning,      nbsp]
                 , UI.div #+ [element elRTSGcElapsedOutdateWarning,  nbsp]
                 , UI.div #+ [element elRTSGcNumOutdateWarning,      nbsp]
                 , UI.div #+ [element elRTSGcMajorNumOutdateWarning, nbsp]
                 ]
             ]
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
  nodeTab       <- UI.button #. [W3BarItem, W3Button] <+> [] # makeItActive #+ [string "Node"]
  peersTab      <- UI.button #. [W3BarItem, W3Button] <+> [] #+ [string "Peers"]
  blockchainTab <- UI.button #. [W3BarItem, W3Button] <+> [] #+ [string "Blockchain"]
  mempoolTab    <- UI.button #. [W3BarItem, W3Button] <+> [] #+ [string "Mempool"]
  barsViewTab   <- UI.anchor #. [W3BarItem, W3Button] <+> [] # set UI.href "#" #+ [UI.string "Bars view"]
  chartsViewTab <- UI.anchor #. [W3BarItem, W3Button] <+> [] # set UI.href "#" #+ [UI.string "Charts view"]
  resourcesTab  <- UI.div #. show W3DropdownHover #+
                     [ UI.button #. show W3Button #+
                         [ string "Resources ▾" ]
                     , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] <+> [] #+
                         [ element barsViewTab
                         , element chartsViewTab
                         ]
                     ]
  ghcRTSTab     <- UI.button #. [W3BarItem, W3Button] <+> [] #+ [string "GHC RTS"]
  errorsTab     <- UI.button #. [W3BarItem, W3Button] <+> [] #+ [string "Errors"]

  let tabs :: [(Element, Element, String)]
      tabs =
        [ (nodeTab,       nodeTabContent,            "Node")
        , (peersTab,      peersTabContent,           "Peers")
        , (blockchainTab, blockchainTabContent,      "Blockchain")
        , (mempoolTab,    mempoolTabContent,         "Mempool")
        , (barsViewTab,   resourcesTabContentBars,   "Bars view")
        , (chartsViewTab, resourcesTabContentCharts, "Charts view")
        , (errorsTab,     errorsTabContent,          "Errors")
        , (ghcRTSTab,     ghcRTSTabContent,          "GHC RTS")
        ]

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
          , element peersTab
          , element blockchainTab
          , element mempoolTab
          , element resourcesTab
          , element errorsTab
          , element ghcRTSTab
          ]
      , element nodeTabContent
      , element peersTabContent
      , element blockchainTabContent
      , element mempoolTabContent
      , element resourcesTabContentBars
      , element resourcesTabContentCharts
      , element errorsTabContent
      , element ghcRTSTabContent
      ]

  -- Return these elements, they will be updated by another thread later.
  let nodeStateElems =
        Map.fromList
          [ (ElNodeRelease,             elNodeRelease)
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
          -- Outdated warnings
          , (ElNodeReleaseOutdateWarning,        elNodeReleaseOutdateWarning)
          , (ElNodeVersionOutdateWarning,        elNodeVersionOutdateWarning)
          , (ElNodePlatformOutdateWarning,       elNodePlatformOutdateWarning)
          , (ElNodeCommitHrefOutdateWarning,     elNodeCommitHrefOutdateWarning)
          , (ElUptimeOutdateWarning,             elUptimeOutdateWarning)
          , (ElOpCertStartKESPeriodOutdateWarning, elOpCertStartKESPeriodOutdateWarning)
          , (ElCurrentKESPeriodOutdateWarning,     elCurrentKESPeriodOutdateWarning)
          , (ElRemainingKESPeriodsOutdateWarning,  elRemainingKESPeriodsOutdateWarning)
          , (ElSlotOutdateWarning,               elSlotOutdateWarning)
          , (ElBlocksNumberOutdateWarning,       elBlocksNumberOutdateWarning)
          , (ElBlocksForgedNumberOutdateWarning, elBlocksForgedNumberOutdateWarning)
          , (ElChainDensityOutdateWarning,       elChainDensityOutdateWarning)
          , (ElNodeIsLeaderNumberOutdateWarning, elNodeIsLeaderNumberOutdateWarning)
          , (ElSlotsMissedNumberOutdateWarning,  elSlotsMissedNumberOutdateWarning)
          , (ElForksCreatedNumberOutdateWarning, elForksCreatedNumberOutdateWarning)
          , (ElRTSGcCpuOutdateWarning,           elRTSGcCpuOutdateWarning)
          , (ElRTSGcElapsedOutdateWarning,       elRTSGcElapsedOutdateWarning)
          , (ElRTSGcNumOutdateWarning,           elRTSGcNumOutdateWarning)
          , (ElRTSGcMajorNumOutdateWarning,      elRTSGcMajorNumOutdateWarning)
          -- Progress bars
          , (ElMempoolBytesProgress,    elMempoolBytesProgress)
          , (ElMempoolBytesProgressBox, elMempoolBytesProgressBox)
          , (ElMempoolTxsProgress,      elMempoolTxsProgress)
          , (ElMempoolTxsProgressBox,   elMempoolTxsProgressBox)
          , (ElMemoryProgress,          elMemoryProgress)
          , (ElMemoryProgressBox,       elMemoryProgressBox)
          , (ElCPUProgress,             elCPUProgress)
          , (ElCPUProgressBox,          elCPUProgressBox)
          , (ElDiskReadProgress,        elDiskUsageRProgress)
          , (ElDiskReadProgressBox,     elDiskUsageRProgressBox)
          , (ElDiskWriteProgress,       elDiskUsageWProgress)
          , (ElDiskWriteProgressBox,    elDiskUsageWProgressBox)
          , (ElNetworkInProgress,       elNetworkUsageInProgress)
          , (ElNetworkInProgressBox,    elNetworkUsageInProgressBox)
          , (ElNetworkOutProgress,      elNetworkUsageOutProgress)
          , (ElNetworkOutProgressBox,   elNetworkUsageOutProgressBox)
          , (ElRTSMemoryProgress,       elRTSMemoryProgress)
          , (ElRTSMemoryProgressBox,    elRTSMemoryProgressBox)
          ]

  return (nodeWidget, nodeStateElems, peerInfoItems)

---

vSpacer :: HTMLClass -> UI Element
vSpacer className = UI.div #. show className #+ []

twoElementsInRow :: UI Element -> UI Element -> UI Element
twoElementsInRow firstOne secondOne =
  UI.div #. show W3Row #+
    [ UI.div #. show W3Half #+ [firstOne]
    , UI.div #. show W3Half #+ [secondOne]
    ]

-- | Since information and metrics are splitted to tabs,
--   we have to make them clickable and show which one is active.
registerClicksOnTabs
  :: [(Element, Element, String)]
  -> UI ()
registerClicksOnTabs tabs =
  forM_ tabs $ \(tab, _, tabName) ->
    void $ UI.onEvent (UI.click tab) $ \_ -> showTabAndMakeItActive tabName
 where
  showTabAndMakeItActive aName =
    forM_ tabs $ \(tab', tabContent, tabName') ->
      if aName == tabName'
        then do
          void $ element tabContent # showIt
          void $ element tab' # makeItActive
        else do
          void $ element tabContent # hideIt
          void $ element tab' # makeItInactive

makeItActive, makeItInactive :: UI Element -> UI Element
makeItActive   = set UI.class_ ([W3BarItem, W3Button] <+> [ActiveTab])
makeItInactive = set UI.class_ ([W3BarItem, W3Button] <+> [])

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. show InfoMark
          #  set UI.title__ aTitle
          #+ [ UI.img #. show InfoMarkImg
                      # set UI.src "/static/images/question-circle-solid.svg" #+ []
             ]

nbsp :: UI Element
nbsp = UI.span # set UI.html "&nbsp;" #+ []
