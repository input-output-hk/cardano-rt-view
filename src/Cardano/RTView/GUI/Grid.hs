{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.GUI.Grid
    ( mkNodesGrid
    , metricLabel
    , allMetricsNames
    ) where

import           Cardano.Prelude
import           Prelude (String)

import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, set, string, ( # ), ( #+ ),
                                              ( #. ))

import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), HTMLW3Class (..),
                                              NodeStateElements, NodesStateElements,
                                              PeerInfoItem (..), ( ## ), (<+>))
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

mkNodesGrid
  :: UI.Window
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements)
mkNodesGrid _window acceptors = do
  nodesEls
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         nodeEls <- mkNodeElements nameOfNode
         return (nameOfNode, nodeEls, [])

  nodesRowCells <- mkNodesRowCells acceptors

  metricRows <-
    forM allMetricsNames $ \aName -> do
      row <- mkRowCells nodesEls aName
      element <$> UI.tr ## show aName #+ row
  let allRows = UI.tr #+ nodesRowCells : metricRows

  nodesGrid
    <- UI.div #. [W3Container, W3Margin] <+> [] #+
         [ UI.div #. show W3Responsive #+
             [ UI.table #. [W3Table, W3Bordered] <+> [] #+
                 allRows
             ]
         ]

  return (nodesGrid, nodesEls)

metricLabel :: ElementName -> (String, String)
metricLabel ElNodeProtocol          = ("Node protocol", "Node's protocol")
metricLabel ElNodeVersion           = ("Node version", "Version of the node")
metricLabel ElNodePlatform          = ("Node platform", "Platform the node is working on")
metricLabel ElNodeCommitHref        = ("Node commit", "Git commit the node was built from")
metricLabel ElUptime                = ("Node uptime", "How long the node is working")
metricLabel ElTraceAcceptorEndpoint = ("Node endpoint", "Socket/pipe used to connect the node with RTView")
metricLabel ElPeersNumber           = ("Peers number", "Number of peers connected to the node")
metricLabel ElOpCertStartKESPeriod  = ("Start KES period", "Certificate KES start period")
metricLabel ElCurrentKESPeriod      = ("Current KES period", "Current KES period")
metricLabel ElRemainingKESPeriods   = ("KES remaining periods", "KES periods until expiry")
metricLabel ElMemoryUsageChart      = ("Memory usage", "Memory used by the node, in MB")
metricLabel ElCPUUsageChart         = ("CPU usage", "CPU used by the node, in percents")
metricLabel ElDiskUsageChart        = ("Disk usage", "Node's disk operations, both READ and WRITE")
metricLabel ElNetworkUsageChart     = ("Network usage", "Node's network operations, both IN and OUT")
metricLabel ElEpoch                 = ("Epoch", "Number of current epoch")
metricLabel ElSlot                  = ("Slot in epoch", "Number of the current slot in this epoch")
metricLabel ElChainDensity          = ("Chain density", "Chain density, in percents")
metricLabel ElBlocksNumber          = ("Blocks number", "Total number of blocks in this blockchain")
metricLabel ElBlocksForgedNumber    = ("Forged blocks number", "Number of blocks forged by this node")
metricLabel ElNodeCannotForge       = ("Cannot forge, number", "Number of slots when this node was a leader but because of misconfiguration, it's impossible to forge a new block")
metricLabel ElNodeIsLeaderNumber    = ("Slot leader, number", "Number of slots when this node was a leader")
metricLabel ElSlotsMissedNumber     = ("Missed slots number", "Number of slots when this node was a leader but didn't forge a new block")
metricLabel ElTxsProcessed          = ("TXs processed", "Number of processed transactions in this blockchain (these transactions are already removed from the mempool")
metricLabel ElMempoolTxsNumber      = ("TXs in mempool, number", "Number of transactions in the mempool")
metricLabel ElMempoolBytes          = ("Txs in mempool, bytes", "Size of all transactions in the mempool, in bytes")
metricLabel ElRTSGcCpu              = ("GC CPU time", "Total CPU time used by the GC, in seconds")
metricLabel ElRTSGcElapsed          = ("GC time elapsed", "Total elapsed time used by the GC, in seconds")
metricLabel ElRTSGcNum              = ("Number of GC runs", "Total number of GCs")
metricLabel ElRTSGcMajorNum         = ("Major GC runs", "Total number of major (oldest generation) GCs")
metricLabel _                       = ("", "")

allMetricsNames :: [ElementName]
allMetricsNames =
  [ ElNodeProtocol
  , ElNodeVersion
  , ElNodePlatform
  , ElNodeCommitHref
  , ElUptime
  , ElTraceAcceptorEndpoint
  , ElPeersNumber
  , ElOpCertStartKESPeriod
  , ElCurrentKESPeriod
  , ElRemainingKESPeriods
  , ElMemoryUsageChart
  , ElCPUUsageChart
  , ElDiskUsageChart
  , ElNetworkUsageChart
  , ElEpoch
  , ElSlot
  , ElBlocksNumber
  , ElBlocksForgedNumber
  , ElNodeCannotForge
  , ElChainDensity
  , ElNodeIsLeaderNumber
  , ElSlotsMissedNumber
  , ElTxsProcessed
  , ElMempoolTxsNumber
  , ElMempoolBytes
  , ElRTSGcCpu
  , ElRTSGcElapsed
  , ElRTSGcNum
  , ElRTSGcMajorNum
  ]

mkNodesRowCells
  :: [RemoteAddrNamed]
  -> UI [UI Element]
mkNodesRowCells acceptors = do
  nodesRowCells
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) ->
         element <$> UI.th ## (show GridNodeTH <> T.unpack nameOfNode) #+
                       [ UI.span #. show GridNodeNameLabel #+ [string "Node: "]
                       , string $ T.unpack nameOfNode
                       ]
  -- To keep top-left corner cell empty.
  emptyRowCell <- element <$> UI.th #+ [UI.span # set UI.html "&nbsp;" #+ []]
  return $ emptyRowCell : nodesRowCells

mkRowCells
  :: [(Text, NodeStateElements, [PeerInfoItem])]
  -> ElementName
  -> UI [UI Element]
mkRowCells nodesElements elemName = do
  tagTd <- element <$> UI.td #+ [string (fst $ metricLabel elemName)
                                        # set UI.title__ (snd $ metricLabel elemName)]
  -- We specify HTML-id for each td because each td corresponds to "node column".
  -- It can be used to hide/show the whole column.
  tds <- forM nodesElements $ \(nameOfNode, nodeElements, _) ->
           element <$> UI.td ## (show elemName <> "-" <> T.unpack nameOfNode)
                             #. show GridRowCell
                             #+ [element $ nodeElements ! elemName]
  return $ tagTd : tds

mkNodeElements
  :: Text
  -> UI NodeStateElements
mkNodeElements nameOfNode = do
  elNodeProtocol <- string "-"
  elNodeVersion  <- string "-"
  elNodePlatform <- string "-"
  elNodeCommitHref
    <- UI.anchor # set UI.href ""
                 # set UI.target "_blank"
                 # set UI.title__ "Browse cardano-node repository on this commit"
                 #+ [string ""]
  elUptime      <- string "00:00:00"
  elTraceAcceptorEndpoint <- string "localhost:0"
  elPeersNumber <- string "0"
  elOpCertStartKESPeriod <- string "-"
  elCurrentKESPeriod     <- string "-"
  elRemainingKESPeriods  <- string "-"

  elMemoryUsageChart
    <- UI.canvas ## (show GridMemoryUsageChartId <> T.unpack nameOfNode)
                 #. show GridMemoryUsageChart
                 #+ []
  elCPUUsageChart
    <- UI.canvas ## (show GridCPUUsageChartId <> T.unpack nameOfNode)
                 #. show GridCPUUsageChart
                 #+ []
  elDiskUsageChart
    <- UI.canvas ## (show GridDiskUsageChartId <> T.unpack nameOfNode)
                 #. show GridDiskUsageChart
                 #+ []
  elNetworkUsageChart
    <- UI.canvas ## (show GridNetworkUsageChartId <> T.unpack nameOfNode)
                 #. show GridNetworkUsageChart
                 #+ []

  elEpoch              <- string "0"
  elSlot               <- string "0"
  elBlocksNumber       <- string "0"
  elBlocksForgedNumber <- string "0"
  elNodeCannotForge    <- string "0"
  elChainDensity       <- string "0"
  elNodeIsLeaderNumber <- string "0"
  elSlotsMissedNumber  <- string "0"
  elTxsProcessed       <- string "0"
  elMempoolTxsNumber   <- string "0"
  elMempoolBytes       <- string "0"
  elRTSGcCpu           <- string "0"
  elRTSGcElapsed       <- string "0"
  elRTSGcNum           <- string "0"
  elRTSGcMajorNum      <- string "0"

  return $
    Map.fromList
      [ (ElNodeProtocol,          elNodeProtocol)
      , (ElNodeVersion,           elNodeVersion)
      , (ElNodePlatform,          elNodePlatform)
      , (ElNodeCommitHref,        elNodeCommitHref)
      , (ElUptime,                elUptime)
      , (ElTraceAcceptorEndpoint, elTraceAcceptorEndpoint)
      , (ElPeersNumber,           elPeersNumber)
      , (ElOpCertStartKESPeriod,  elOpCertStartKESPeriod)
      , (ElCurrentKESPeriod,      elCurrentKESPeriod)
      , (ElRemainingKESPeriods,   elRemainingKESPeriods)
      , (ElMemoryUsageChart,      elMemoryUsageChart)
      , (ElCPUUsageChart,         elCPUUsageChart)
      , (ElDiskUsageChart,        elDiskUsageChart)
      , (ElNetworkUsageChart,     elNetworkUsageChart)
      , (ElEpoch,                 elEpoch)
      , (ElSlot,                  elSlot)
      , (ElBlocksNumber,          elBlocksNumber)
      , (ElBlocksForgedNumber,    elBlocksForgedNumber)
      , (ElNodeCannotForge,       elNodeCannotForge)
      , (ElChainDensity,          elChainDensity)
      , (ElNodeIsLeaderNumber,    elNodeIsLeaderNumber)
      , (ElSlotsMissedNumber,     elSlotsMissedNumber)
      , (ElTxsProcessed,          elTxsProcessed)
      , (ElMempoolTxsNumber,      elMempoolTxsNumber)
      , (ElMempoolBytes,          elMempoolBytes)
      , (ElRTSGcCpu,              elRTSGcCpu)
      , (ElRTSGcElapsed,          elRTSGcElapsed)
      , (ElRTSGcNum,              elRTSGcNum)
      , (ElRTSGcMajorNum,         elRTSGcMajorNum)
      ]
