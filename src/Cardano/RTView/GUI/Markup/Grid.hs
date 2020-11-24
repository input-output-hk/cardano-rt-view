{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.GUI.Markup.Grid
    ( mkNodesGrid
    , metricLabel
    , allMetricsNames
    ) where

import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad (forM)
import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, (#), (#+))

import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), NodeStateElements,
                                              NodesStateElements, PeerInfoItem (..),
                                              (##), (#.), hideIt)
import           Cardano.RTView.NodeState.Types

mkNodesGrid
  :: TVar NodesState
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements)
mkNodesGrid nsTVar acceptors = do
  nodesState <- liftIO $ readTVarIO nsTVar

  nodesEls'
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         idleTag <- string "Idle" #. [IdleNode] # hideIt
         nodeEls <- mkNodeElements (nodesState ! nameOfNode) nameOfNode idleTag acceptors
         return (nameOfNode, nodeEls, [], idleTag)

  let idleTags = [idleTag      | (_,  _,   _, idleTag) <- nodesEls']
      nodesEls = [(nm, els, l) | (nm, els, l, _)       <- nodesEls']
  nodesRowCells <- mkNodesRowCells acceptors idleTags

  metricRows <-
    forM allMetricsNames $ \aName -> do
      row <- mkRowCells nodesEls aName
      element <$> UI.tr ## show aName #+ row
  let allRows = UI.tr #+ nodesRowCells : metricRows

  nodesGrid
    <- UI.div #. [W3Container, W3Margin] #+
         [ UI.div #. [W3Responsive] #+
             [ UI.table #. [W3Table, W3Bordered] #+
                 allRows
             ]
         ]

  return (nodesGrid, nodesEls)

metricLabel :: ElementName -> (String, String)
metricLabel ElNodeProtocol          = ("Node protocol", "Node's protocol")
metricLabel ElNodeVersion           = ("Node version", "Version of the node")
metricLabel ElNodePlatform          = ("Node platform", "Platform the node is working on")
metricLabel ElNodeCommitHref        = ("Node commit", "Git commit the node was built from")
metricLabel ElNodeStarttime         = ("Node start time", "The time when this node has started")
metricLabel ElNodeUptime            = ("Node uptime", "How long the node is working")
metricLabel ElTraceAcceptorEndpoint = ("Node endpoint", "Socket/pipe used to connect the node with RTView")
metricLabel ElPeersNumber           = ("Peers number", "Number of peers connected to the node")
metricLabel ElOpCertStartKESPeriod  = ("Start KES period", "Certificate KES start period")
metricLabel ElOpCertExpiryKESPeriod = ("Expiry KES period", "Certificate KES expiry period")
metricLabel ElCurrentKESPeriod      = ("Current KES period", "Current KES period")
metricLabel ElRemainingKESPeriods   = ("KES remaining periods", "KES periods until expiry")
metricLabel ElRemainingKESPeriodsInDays = ("KES remaining periods, days", "KES periods until expiry, in days")
metricLabel ElMemoryUsageChart      = ("Memory usage", "Memory used by the node, in MB")
metricLabel ElCPUUsageChart         = ("CPU usage", "CPU used by the node, in percents")
metricLabel ElDiskUsageChart        = ("Disk usage", "Node's disk operations, both READ and WRITE")
metricLabel ElNetworkUsageChart     = ("Network usage", "Node's network operations, both IN and OUT")
metricLabel ElSystemStartTime       = ("Blockchain start time", "The time when this blockchain has started")
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
  , ElNodeStarttime
  , ElNodeUptime
  , ElTraceAcceptorEndpoint
  , ElPeersNumber
  , ElOpCertStartKESPeriod
  , ElOpCertExpiryKESPeriod
  , ElCurrentKESPeriod
  , ElRemainingKESPeriods
  , ElRemainingKESPeriodsInDays
  , ElMemoryUsageChart
  , ElCPUUsageChart
  , ElDiskUsageChart
  , ElNetworkUsageChart
  , ElSystemStartTime
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
  -> [Element]
  -> UI [UI Element]
mkNodesRowCells acceptors idleTags = do
  let acceptorsWithTags = zip acceptors idleTags
  nodesRowCells
    <- forM acceptorsWithTags $ \(RemoteAddrNamed nameOfNode _, idleTag) ->
         element <$> UI.th ## (show GridNodeTH <> T.unpack nameOfNode) #+
                       [ string "Node: " #. [GridNodeNameLabel]
                       , string $ T.unpack nameOfNode
                       , element idleTag
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
                             #. [GridRowCell]
                             #+ [element $ nodeElements ! elemName]
  return $ tagTd : tds

mkNodeElements
  :: NodeState
  -> Text
  -> Element
  -> [RemoteAddrNamed]
  -> UI NodeStateElements
mkNodeElements NodeState {..} nameOfNode elIdleNode acceptors = do
  let PeerMetrics {..}       = peersMetrics
      MempoolMetrics {..}    = mempoolMetrics
      ForgeMetrics {..}      = forgeMetrics
      RTSMetrics {..}        = rtsMetrics
      BlockchainMetrics {..} = blockchainMetrics
      KESMetrics {..}        = kesMetrics
      NodeMetrics {..}       = nodeMetrics

  let acceptorEndpoint = mkTraceAcceptorEndpoint nameOfNode acceptors

  elNodeProtocol              <- string $ showText nodeProtocol
  elNodeVersion               <- string $ showText nodeVersion
  elNodePlatform              <- string $ showText nodePlatform
  elNodeStarttime             <- string $ showTime nodeStartTime
  elNodeUptime                <- string   showInitTime
  elTraceAcceptorEndpoint     <- string   acceptorEndpoint
                                        # set UI.title__ (fullEndpointTitle acceptorEndpoint)
  elPeersNumber               <- string $ showInteger (fromIntegral $ length peersInfo)
  elOpCertStartKESPeriod      <- string $ showInteger opCertStartKESPeriod
  elOpCertExpiryKESPeriod     <- string $ showInteger opCertExpiryKESPeriod
  elCurrentKESPeriod          <- string $ showInteger currentKESPeriod
  elRemainingKESPeriods       <- string $ showInteger remKESPeriods
  elRemainingKESPeriodsInDays <- string $ showInteger remKESPeriodsInDays
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
  elMempoolTxsNumber          <- string $ showInteger mempoolTxsNumber
  elMempoolBytes              <- string $ showWord64  mempoolBytes
  elRTSGcCpu                  <- string $ showDouble  rtsGcCpu
  elRTSGcElapsed              <- string $ showDouble  rtsGcElapsed
  elRTSGcNum                  <- string $ showInteger rtsGcNum
  elRTSGcMajorNum             <- string $ showInteger rtsGcMajorNum

  elMemoryUsageChart
    <- UI.canvas ## (show GridMemoryUsageChartId <> T.unpack nameOfNode)
                 #. [GridMemoryUsageChart]
                 #+ []
  elCPUUsageChart
    <- UI.canvas ## (show GridCPUUsageChartId <> T.unpack nameOfNode)
                 #. [GridCPUUsageChart]
                 #+ []
  elDiskUsageChart
    <- UI.canvas ## (show GridDiskUsageChartId <> T.unpack nameOfNode)
                 #. [GridDiskUsageChart]
                 #+ []
  elNetworkUsageChart
    <- UI.canvas ## (show GridNetworkUsageChartId <> T.unpack nameOfNode)
                 #. [GridNetworkUsageChart]
                 #+ []

  elNodeCommitHref
    <- UI.anchor # set UI.href ""
                 # set UI.target "_blank"
                 # set UI.title__ "Browse cardano-node repository on this commit"
                 # set UI.text (showText nodeShortCommit)

  return $ HM.fromList
    [ (ElIdleNode,              elIdleNode)
    , (ElNodeProtocol,          elNodeProtocol)
    , (ElNodeVersion,           elNodeVersion)
    , (ElNodePlatform,          elNodePlatform)
    , (ElNodeCommitHref,        elNodeCommitHref)
    , (ElNodeStarttime,         elNodeStarttime)
    , (ElNodeUptime,            elNodeUptime)
    , (ElTraceAcceptorEndpoint, elTraceAcceptorEndpoint)
    , (ElPeersNumber,           elPeersNumber)
    , (ElOpCertStartKESPeriod,  elOpCertStartKESPeriod)
    , (ElOpCertExpiryKESPeriod, elOpCertExpiryKESPeriod)
    , (ElCurrentKESPeriod,      elCurrentKESPeriod)
    , (ElRemainingKESPeriods,   elRemainingKESPeriods)
    , (ElRemainingKESPeriodsInDays, elRemainingKESPeriodsInDays)
    , (ElMemoryUsageChart,      elMemoryUsageChart)
    , (ElCPUUsageChart,         elCPUUsageChart)
    , (ElDiskUsageChart,        elDiskUsageChart)
    , (ElNetworkUsageChart,     elNetworkUsageChart)
    , (ElSystemStartTime,       elSystemStartTime)
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
