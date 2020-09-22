{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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

metricLabel :: ElementName -> String
metricLabel ElTraceAcceptorEndpoint = "TraceAcceptor endpoint"
metricLabel ElNodeRelease           = "Node release"
metricLabel ElNodeVersion           = "Node version"
metricLabel ElNodePlatform          = "Node platform"
metricLabel ElNodeCommitHref        = "Node commit"
metricLabel ElPeersNumber           = "Peers number"
metricLabel ElUptime                = "Node uptime"
metricLabel ElOpCertStartKESPeriod  = "Start KES period"
metricLabel ElCurrentKESPeriod      = "Current KES period"
metricLabel ElRemainingKESPeriods   = "KES remaining periods"
metricLabel ElMemoryUsageChart      = "Memory usage"
metricLabel ElCPUUsageChart         = "CPU usage"
metricLabel ElDiskUsageChart        = "Disk usage"
metricLabel ElNetworkUsageChart     = "Network usage"
metricLabel ElEpoch                 = "Epoch"
metricLabel ElSlot                  = "Slot in epoch"
metricLabel ElChainDensity          = "Chain density"
metricLabel ElBlocksNumber          = "Blocks number"
metricLabel ElBlocksForgedNumber    = "Forged blocks number"
metricLabel ElNodeCannotLead        = "Cannot lead, number"
metricLabel ElNodeIsLeaderNumber    = "Slot leader, number"
metricLabel ElSlotsMissedNumber     = "Missed slots number"
metricLabel ElTxsProcessed          = "TXs processed"
metricLabel ElMempoolTxsNumber      = "TXs in mempool, number"
metricLabel ElMempoolBytes          = "Txs in mempool, bytes"
metricLabel ElRTSGcCpu              = "GC CPU time"
metricLabel ElRTSGcElapsed          = "GC time elapsed"
metricLabel ElRTSGcNum              = "Number of GC runs"
metricLabel ElRTSGcMajorNum         = "Major GC runs"
metricLabel _                       = ""

allMetricsNames :: [ElementName]
allMetricsNames =
  [ ElTraceAcceptorEndpoint
  , ElNodeRelease
  , ElNodeVersion
  , ElNodePlatform
  , ElNodeCommitHref
  , ElPeersNumber
  , ElUptime
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
  , ElNodeCannotLead
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
  return $ [emptyRowCell] ++ nodesRowCells

mkRowCells
  :: [(Text, NodeStateElements, [PeerInfoItem])]
  -> ElementName
  -> UI [UI Element]
mkRowCells nodesElements elemName = do
  tagTd <- element <$> UI.td #+ [string $ metricLabel elemName]
  -- We specify HTML-id for each td because each td corresponds to "node column".
  -- It can be used to hide/show the whole column.
  tds <- forM nodesElements $ \(nameOfNode, nodeElements, _) ->
           element <$> UI.td ## (show elemName <> "-" <> T.unpack nameOfNode)
                             #. show GridRowCell
                             #+ [element $ nodeElements ! elemName]
  return $ [tagTd] ++ tds

mkNodeElements
  :: Text
  -> UI NodeStateElements
mkNodeElements nameOfNode = do
  elTraceAcceptorEndpoint <- string "localhost:0"
  elNodeRelease  <- string "-"
  elNodeVersion  <- string "-"
  elNodePlatform <- string "-"
  elNodeCommitHref
    <- UI.anchor # set UI.href ""
                 # set UI.target "_blank"
                 # set UI.title__ "Browse cardano-node repository on this commit"
                 #+ [string ""]
  elPeersNumber <- string "0"
  elUptime      <- string "00:00:00"
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
  elNodeCannotLead     <- string "0"
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
      [ (ElTraceAcceptorEndpoint, elTraceAcceptorEndpoint)
      , (ElNodeRelease,           elNodeRelease)
      , (ElNodeVersion,           elNodeVersion)
      , (ElNodePlatform,          elNodePlatform)
      , (ElNodeCommitHref,        elNodeCommitHref)
      , (ElPeersNumber,           elPeersNumber)
      , (ElUptime,                elUptime)
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
      , (ElNodeCannotLead,        elNodeCannotLead)
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
