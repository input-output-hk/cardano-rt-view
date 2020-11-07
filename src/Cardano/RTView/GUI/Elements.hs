{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.RTView.GUI.Elements
    ( HTMLClass (..)
    , HTMLId (..)
    , NodesStateElements
    , NodeStateElements
    , ElementName (..)
    , ElementValue (..)
    , PeerInfoItem (..)
    , PeerInfoElements (..)
    , (#.)
    , (##)
    , showIt
    , hideIt
    , showRow
    , showCell
    ) where

import           Control.DeepSeq (NFData (..), rwhnf)
import           GHC.Generics (Generic)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, (#))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Data.Word (Word64)

instance NFData Element where
  rnf = rwhnf

-- | GUI elements containing current node state (info, metrics).
--   These elements are continuously updating using |LogObject|s
--   received by |TraceAcceptor|s.
type NodeStateElements = Map ElementName Element

-- | GUI elements for all nodes, tuples from nodeName, its elements and prepared peers items.
type NodesStateElements = [(Text, NodeStateElements, [PeerInfoItem])]

data ElementName
  = ElNodeProtocol
  | ElNodeVersion
  | ElNodePlatform
  | ElNodeCommitHref
  | ElActiveNode
  | ElUptime
  | ElSystemStartTime
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
  | ElOpCertExpiryKESPeriod
  | ElCurrentKESPeriod
  | ElRemainingKESPeriods
  | ElRemainingKESPeriodsInDays
  | ElNodeErrors
  | ElNodeErrorsTab
  | ElMempoolTxsNumber
  | ElMempoolTxsPercent
  | ElMempoolBytes
  | ElMempoolBytesPercent
  | ElMempoolMaxTxs
  | ElMempoolMaxBytes
  | ElCPULast
  | ElCPUNs
  | ElDiskUsageR
  | ElDiskUsageRMax
  | ElDiskUsageRPercent
  | ElDiskUsageW
  | ElDiskUsageWMax
  | ElDiskUsageWPercent
  | ElNetworkUsageIn
  | ElNetworkUsageOut
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
  | ElementText    !Text
  deriving (Generic, NFData)

-- | An item for each connected peer, contains a parent element
--   and list of child elements.
data PeerInfoItem = PeerInfoItem
  { piItem      :: !Element
  , piItemElems :: !PeerInfoElements
  } deriving (Generic, NFData)

data PeerInfoElements = PeerInfoElements
  { pieEndpoint   :: !Element
  , pieBytesInF   :: !Element
  , pieReqsInF    :: !Element
  , pieBlocksInF  :: !Element
  , pieSlotNumber :: !Element
  , pieStatus     :: !Element
  } deriving (Generic, NFData)

-- | HTML elements identifiers, we use them in HTML, CSS and JS FFI.

data HTMLClass
  = NoClass
  | ActiveTab
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
  | ResourcesIcon
  | SelectMetricCheck
  | SelectMetricCheckArea
  | SelectNodeCheck
  | SelectNodeCheckArea
  | ServiceName
  | ShowHideIcon
  | TabContainer
  | TopBar
  | TXsProcessed
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
  -- W3C classes
  | W3Bar
  | W3BarBlock
  | W3BarItem
  | W3Border
  | W3Bordered
  | W3BorderBottom
  | W3BorderTop
  | W3Button
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

instance Show HTMLClass where
  show NoClass                = ""
  show ActiveTab              = "ActiveTab"
  show BarValueUnit           = "BarValueUnit"
  show CardanoLogo            = "CardanoLogo"
  show CommitLink             = "CommitLink"
  show DensityPercent         = "DensityPercent"
  show ErrorsTabContainer     = "ErrorsTabContainer"
  show GridNodeNameLabel      = "GridNodeNameLabel"
  show GridRowCell            = "GridRowCell"
  show HSpacer                = "HSpacer"
  show InfoMark               = "InfoMark"
  show InfoMarkImg            = "InfoMarkImg"
  show MetricsArea            = "MetricsArea"
  show NodeContainer          = "NodeContainer"
  show NodeBar                = "NodeBar"
  show NodeInfoValues         = "NodeInfoValues"
  show NodeInfoVSpacer        = "NodeInfoVSpacer"
  show NodeMetricsValues      = "NodeMetricsValues"
  show NodeMetricsVSpacer     = "NodeMetricsVSpacer"
  show NodeMenuIcon           = "NodeMenuIcon"
  show NodeName               = "NodeName"
  show NodeNameArea           = "NodeNameArea"
  show OutdatedValue          = "OutdatedValue"
  show PercentsSlashHSpacer   = "PercentsSlashHSpacer"
  show PercentsSlashHRSpacer  = "PercentsSlashHRSpacer"
  show ProgressBar            = "ProgressBar"
  show ProgressBarOutdated    = "ProgressBarOutdated"
  show ProgressBarBox         = "ProgressBarBox"
  show ProgressBarBoxOutdated = "ProgressBarBoxOutdated"
  show ResourcesIcon          = "ResourcesIcon"
  show SelectMetricCheck      = "SelectMetricCheck"
  show SelectMetricCheckArea  = "SelectMetricCheckArea"
  show SelectNodeCheck        = "SelectNodeCheck"
  show SelectNodeCheckArea    = "SelectNodeCheckArea"
  show ServiceName            = "ServiceName"
  show ShowHideIcon           = "ShowHideIcon"
  show TabContainer           = "TabContainer"
  show TopBar                 = "TopBar"
  show TXsProcessed           = "TXsProcessed"
  show ValueUnit              = "ValueUnit"
  show ValueUnitPercent       = "ValueUnitPercent"
  show CPUUsageChart          = "CPUUsageChart"
  show MemoryUsageChart       = "MemoryUsageChart"
  show DiskUsageChart         = "DiskUsageChart"
  show NetworkUsageChart      = "NetworkUsageChart"
  show GridCPUUsageChart      = "GridCPUUsageChart"
  show GridMemoryUsageChart   = "GridMemoryUsageChart"
  show GridDiskUsageChart     = "GridDiskUsageChart"
  show GridNetworkUsageChart  = "GridNetworkUsageChart"
  show WarningMessage         = "WarningMessage"
  show ErrorMessage           = "ErrorMessage"
  show CriticalMessage        = "CriticalMessage"
  show AlertMessage           = "AlertMessage"
  show EmergencyMessage       = "EmergencyMessage"
  -- Names of these classes are taken from W3C-library.
  show W3Bar             = "w3-bar"
  show W3BarBlock        = "w3-bar-block"
  show W3BarItem         = "w3-bar-item"
  show W3Border          = "w3-border"
  show W3Bordered        = "w3-bordered"
  show W3BorderBottom    = "w3-border-bottom"
  show W3BorderTop       = "w3-border-top"
  show W3Button          = "w3-button"
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

data HTMLId
  = SelectMetricButton
  | HideAllMetricsButton
  | ShowAllMetricsButton
  | HideAllNodesButton
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

(#.) :: UI Element -> [HTMLClass] -> UI Element
(#.) el []   = el # UI.set UI.class_ ""
(#.) el [cl] = el # UI.set UI.class_ (show cl)
(#.) el cls  = el # UI.set UI.class_ (unwords $ map show cls)

showIt, hideIt, showRow, showCell :: UI Element -> UI Element
showIt   = UI.set UI.style [("display", "block")]
hideIt   = UI.set UI.style [("display", "none")]
showRow  = UI.set UI.style [("display", "table-row")]
showCell = UI.set UI.style [("display", "table-cell")]
