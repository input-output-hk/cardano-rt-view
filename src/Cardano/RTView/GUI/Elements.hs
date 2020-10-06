{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.RTView.GUI.Elements
    ( HTMLClass (..)
    , HTMLW3Class (..)
    , HTMLId (..)
    , NodesStateElements
    , NodeStateElements
    , ElementName (..)
    , ElementValue (..)
    , PeerInfoItem (..)
    , PeerInfoElements (..)
    , (<+>)
    , (##)
    , showIt
    , hideIt
    , showRow
    , showCell
    ) where

import           Cardano.Prelude
import           Control.DeepSeq (NFData (..), rwhnf)
import           Data.Map.Strict (Map)
import           Prelude (Show (..), String)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, ( # ))

instance NFData Element where
  rnf = rwhnf

-- | GUI elements containing current node state (info, metrics).
--   These elements are continuously updating using |LogObject|s
--   received by |TraceAcceptor|s.
type NodeStateElements = Map ElementName Element

-- | GUI elements for all nodes, tuples from nodeName, its elements and prepared peers items.
type NodesStateElements = [(Text, NodeStateElements, [PeerInfoItem])]

data ElementName
  = ElNodeRelease
  | ElNodeVersion
  | ElNodePlatform
  | ElNodeCommitHref
  | ElActiveNode
  | ElUptime
  | ElEpoch
  | ElSlot
  | ElBlocksNumber
  | ElBlocksForgedNumber
  | ElNodeCannotForge
  | ElChainDensity
  | ElNodeIsLeaderNumber
  | ElSlotsMissedNumber
  | ElTxsProcessed
  | ElPeersNumber
  | ElTraceAcceptorHost
  | ElTraceAcceptorPort
  | ElTraceAcceptorEndpoint
  | ElOpCertStartKESPeriod
  | ElCurrentKESPeriod
  | ElRemainingKESPeriods
  | ElNodeErrors
  | ElMempoolTxsNumber
  | ElMempoolTxsPercent
  | ElMempoolBytes
  | ElMempoolBytesPercent
  | ElMempoolMaxTxs
  | ElMempoolMaxBytes
  | ElMemory
  | ElMemoryMax
  | ElMemoryMaxTotal
  | ElMemoryPercent
  | ElCPUPercent
  | ElCPULast
  | ElCPUNs
  | ElDiskUsageR
  | ElDiskUsageRMax
  | ElDiskUsageRMaxTotal
  | ElDiskUsageRPercent
  | ElDiskUsageW
  | ElDiskUsageWMax
  | ElDiskUsageWMaxTotal
  | ElDiskUsageWPercent
  | ElNetworkUsageIn
  | ElNetworkUsageInMaxTotal
  | ElNetworkUsageOut
  | ElNetworkUsageOutMaxTotal
  | ElRTSMemoryAllocated
  | ElRTSMemoryUsed
  | ElRTSMemoryUsedPercent
  | ElRTSGcCpu
  | ElRTSGcElapsed
  | ElRTSGcNum
  | ElRTSGcMajorNum
  -- Progress bars.
  | ElMempoolBytesProgress
  | ElMempoolBytesProgressBox
  | ElMempoolTxsProgress
  | ElMempoolTxsProgressBox
  | ElRTSMemoryProgress
  | ElRTSMemoryProgressBox
  -- Charts
  | ElMemoryUsageChart
  | ElCPUUsageChart
  | ElDiskUsageChart
  | ElNetworkUsageChart
  deriving (Eq, Generic, NFData, Ord, Show)

data ElementValue
  = ElementInt     !Int
  | ElementInteger !Integer
  | ElementWord64  !Word64
  | ElementDouble  !Double
  | ElementString  !String
  deriving (Generic, NFData)

-- | An item for each connected peer, contains a parent element
--   and list of child elements.
data PeerInfoItem
  = PeerInfoItem
      { piItem      :: !Element
      , piItemElems :: !PeerInfoElements
      }
  deriving (Generic, NFData)

data PeerInfoElements
  = PeerInfoElements
      { pieEndpoint   :: !Element
      , pieBytesInF   :: !Element
      , pieReqsInF    :: !Element
      , pieBlocksInF  :: !Element
      , pieSlotNumber :: !Element
      , pieStatus     :: !Element
      }
  deriving (Generic, NFData)

-- | HTML elements identifiers, we use them in HTML, CSS and JS FFI.

data HTMLClass
  = ActiveTab
  | BarValueUnit
  | CardanoLogo
  | CommitLink
  | DensityPercent
  | ErrorsTabContainer
  | GridNodeNameLabel
  | GridRowCell
  | HSpacer
  | InfoMark
  | InfoMarkImg
  | MetricsArea
  | NodeContainer
  | NodeBar
  | NodeInfoValues
  | NodeInfoVSpacer
  | NodeMetricsValues
  | NodeMetricsVSpacer
  | NodeMenuIcon
  | NodeName
  | NodeNameArea
  | OutdatedValue
  | PercentsSlashHSpacer
  | PercentsSlashHRSpacer
  | ProgressBar
  | ProgressBarOutdated
  | ProgressBarBox
  | ProgressBarBoxOutdated
  | ReleaseName
  | SelectMetricCheck
  | SelectMetricCheckArea
  | SelectNodeCheck
  | SelectNodeCheckArea
  | ServiceName
  | TabContainer
  | TopBar
  | ValueUnit
  | ValueUnitPercent
  -- Charts
  | CPUUsageChart
  | MemoryUsageChart
  | DiskUsageChart
  | NetworkUsageChart
  | GridCPUUsageChart
  | GridMemoryUsageChart
  | GridDiskUsageChart
  | GridNetworkUsageChart
  -- Error messages
  | WarningMessage
  | ErrorMessage
  | CriticalMessage
  | AlertMessage
  | EmergencyMessage
  deriving Show

data HTMLW3Class
  = W3Bar
  | W3BarBlock
  | W3BarItem
  | W3Border
  | W3Bordered
  | W3BorderTop
  | W3Button
  | W3Card4
  | W3Check
  | W3Col
  | W3Container
  | W3Disabled
  | W3DropdownContent
  | W3DropdownHover
  | W3Half
  | W3HideMedium
  | W3HideSmall
  | W3Large
  | W3Margin
  | W3Mobile
  | W3Responsive
  | W3Rest
  | W3Right
  | W3RightAlign
  | W3Row
  | W3Sidebar
  | W3Table
  | W3Theme
  | W3Third
  | W3TwoThird
  | W3Quarter
  | W3L6
  | W3M12
  | W3S12

-- | We have to provide explicit Show-instance,
--   because all these classes are taken from W3C-library.
instance Show HTMLW3Class where
  show W3Bar             = "w3-bar"
  show W3BarBlock        = "w3-bar-block"
  show W3BarItem         = "w3-bar-item"
  show W3Border          = "w3-border"
  show W3Bordered        = "w3-bordered"
  show W3BorderTop       = "w3-border-top"
  show W3Button          = "w3-button"
  show W3Card4           = "w3-card-4"
  show W3Check           = "w3-check"
  show W3Col             = "w3-col"
  show W3Container       = "w3-container"
  show W3Disabled        = "w3-disabled"
  show W3DropdownContent = "w3-dropdown-content"
  show W3DropdownHover   = "w3-dropdown-hover"
  show W3Half            = "w3-half"
  show W3HideMedium      = "w3-hide-medium"
  show W3HideSmall       = "w3-hide-small"
  show W3Large           = "w3-large"
  show W3Margin          = "w3-margin"
  show W3Mobile          = "w3-mobile"
  show W3Responsive      = "w3-responsive"
  show W3Rest            = "w3-rest"
  show W3Right           = "w3-right"
  show W3RightAlign      = "w3-right-align"
  show W3Row             = "w3-row"
  show W3Sidebar         = "w3-sidebar"
  show W3Table           = "w3-table"
  show W3Theme           = "w3-theme"
  show W3Third           = "w3-third"
  show W3TwoThird        = "w3-twothird"
  show W3Quarter         = "w3-quarter"
  show W3L6              = "l6"
  show W3M12             = "m12"
  show W3S12             = "s12"

-- | Operator for class names concatenation. Please note
--   that w3-classes should be the first, because our
--   own classes override some of them.
(<+>) :: [HTMLW3Class] -> [HTMLClass] -> String
(<+>) w3Classes ownClasses = concat $ intersperse " " allClasses
 where
  allClasses = map Prelude.show w3Classes ++ map Prelude.show ownClasses

data HTMLId
  = SelectMetricButton
  | ShowAllMetricsButton
  | ShowAllNodesButton
  | ViewModeButton
  -- Id parts (the final id will be formed using unique name of node).
  | CPUUsageChartId
  | DiskUsageChartId
  | MemoryUsageChartId
  | NetworkUsageChartId
  | GridCPUUsageChartId
  | GridDiskUsageChartId
  | GridMemoryUsageChartId
  | GridNetworkUsageChartId
  | GridNodeTH
  deriving Show

(##) :: UI Element -> String  -> UI Element
(##) el i = el # UI.set UI.id_ i

showIt, hideIt, showRow, showCell :: UI Element -> UI Element
showIt   = UI.set UI.style [("display", "block")]
hideIt   = UI.set UI.style [("display", "none")]
showRow  = UI.set UI.style [("display", "table-row")]
showCell = UI.set UI.style [("display", "table-cell")]
