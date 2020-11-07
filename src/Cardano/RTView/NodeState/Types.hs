{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.NodeState.Types
    ( NodesState
    , PeerMetrics (..)
    , MempoolMetrics (..)
    , ForgeMetrics (..)
    , ResourcesMetrics (..)
    , RTSMetrics (..)
    , BlockchainMetrics (..)
    , KESMetrics (..)
    , NodeMetrics (..)
    , ErrorsMetrics (..)
    , NodeState (..)
    , NodeError (..)
    , PeerInfo (..)
    , defaultNodesState
    ) where

import           Control.DeepSeq (NFData (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock (UTCTime (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))
import           Cardano.BM.Data.Severity (Severity)

type NodesState = Map Text NodeState

data PeerInfo = PeerInfo
  { piEndpoint   :: !String
  , piBytesInF   :: !String
  , piReqsInF    :: !String
  , piBlocksInF  :: !String
  , piSlotNumber :: !String
  , piStatus     :: !String
  } deriving (Eq, Generic, NFData)

-- Severity type already has Generic instance.
instance NFData Severity

data NodeError = NodeError
  { eTimestamp :: !UTCTime
  , eSeverity  :: !Severity
  , eMessage   :: !String
  } deriving (Generic, NFData)

data NodeState = NodeState
  { peersMetrics      :: !PeerMetrics
  , mempoolMetrics    :: !MempoolMetrics
  , forgeMetrics      :: !ForgeMetrics
  , resourcesMetrics  :: !ResourcesMetrics
  , rtsMetrics        :: !RTSMetrics
  , blockchainMetrics :: !BlockchainMetrics
  , kesMetrics        :: !KESMetrics
  , nodeMetrics       :: !NodeMetrics
  , nodeErrors        :: !ErrorsMetrics
  } deriving (Generic, NFData)

data PeerMetrics = PeerMetrics
  { peersInfo :: ![PeerInfo]
  , peersInfoChanged :: !Bool
  } deriving (Generic, NFData)

data MempoolMetrics = MempoolMetrics
  { mempoolTxsNumber        :: !Word64
  , mempoolTxsNumberChanged :: !Bool
  , mempoolTxsPercent       :: !Double
  , mempoolBytes            :: !Word64
  , mempoolBytesChanged     :: !Bool
  , mempoolBytesPercent     :: !Double
  , mempoolMaxTxs           :: !Integer
  , mempoolMaxBytes         :: !Integer
  , txsProcessed            :: !Integer
  , txsProcessedChanged     :: !Bool
  } deriving (Generic, NFData)

data ForgeMetrics = ForgeMetrics
  { nodeIsLeaderNum              :: !Integer
  , nodeIsLeaderNumChanged       :: !Bool
  , nodeIsLeaderNumLastUpdate    :: !Word64
  , slotsMissedNumber            :: !Integer
  , slotsMissedNumberChanged     :: !Bool
  , slotsMissedNumberLastUpdate  :: !Word64
  , nodeCannotForge              :: !Integer
  , nodeCannotForgeChanged       :: !Bool
  , blocksForgedNumber           :: !Integer
  , blocksForgedNumberChanged    :: !Bool
  , blocksForgedNumberLastUpdate :: !Word64
  } deriving (Generic, NFData)

data ResourcesMetrics = ResourcesMetrics
  { memory                    :: !Double
  , memoryChanged             :: !Bool
  , memoryMax                 :: !Double
  , memoryMaxTotal            :: !Double
  , memoryPercent             :: !Double
  , memoryLastUpdate          :: !Word64
  , cpuPercent                :: !Double
  , cpuPercentChanged         :: !Bool
  , cpuLast                   :: !Integer
  , cpuNs                     :: !Word64
  , cpuLastUpdate             :: !Word64
  , diskUsageR                :: !Double
  , diskUsageRChanged         :: !Bool
  , diskUsageRMax             :: !Double
  , diskUsageRMaxTotal        :: !Double
  , diskUsageRPercent         :: !Double
  , diskUsageRLast            :: !Word64
  , diskUsageRNs              :: !Word64
  , diskUsageRAdaptTime       :: !UTCTime
  , diskUsageRLastUpdate      :: !Word64
  , diskUsageW                :: !Double
  , diskUsageWChanged         :: !Bool
  , diskUsageWMax             :: !Double
  , diskUsageWMaxTotal        :: !Double
  , diskUsageWPercent         :: !Double
  , diskUsageWLast            :: !Word64
  , diskUsageWNs              :: !Word64
  , diskUsageWAdaptTime       :: !UTCTime
  , diskUsageWLastUpdate      :: !Word64
  , networkUsageIn            :: !Double
  , networkUsageInChanged     :: !Bool
  , networkUsageInPercent     :: !Double
  , networkUsageInMax         :: !Double
  , networkUsageInMaxTotal    :: !Double
  , networkUsageInLast        :: !Word64
  , networkUsageInNs          :: !Word64
  , networkUsageInLastUpdate  :: !Word64
  , networkUsageOut           :: !Double
  , networkUsageOutChanged    :: !Bool
  , networkUsageOutPercent    :: !Double
  , networkUsageOutMax        :: !Double
  , networkUsageOutMaxTotal   :: !Double
  , networkUsageOutLast       :: !Word64
  , networkUsageOutNs         :: !Word64
  , networkUsageOutLastUpdate :: !Word64
  } deriving (Generic, NFData)

data RTSMetrics = RTSMetrics
  { rtsMemoryAllocated        :: !Double
  , rtsMemoryAllocatedChanged :: !Bool
  , rtsMemoryUsed             :: !Double
  , rtsMemoryUsedChanged      :: !Bool
  , rtsMemoryUsedPercent      :: !Double
  , rtsMemoryLastUpdate       :: !Word64
  , rtsGcCpu                  :: !Double
  , rtsGcCpuChanged           :: !Bool
  , rtsGcCpuLastUpdate        :: !Word64
  , rtsGcElapsed              :: !Double
  , rtsGcElapsedChanged       :: !Bool
  , rtsGcElapsedLastUpdate    :: !Word64
  , rtsGcNum                  :: !Integer
  , rtsGcNumChanged           :: !Bool
  , rtsGcNumLastUpdate        :: !Word64
  , rtsGcMajorNum             :: !Integer
  , rtsGcMajorNumChanged      :: !Bool
  , rtsGcMajorNumLastUpdate   :: !Word64
  } deriving (Generic, NFData)

data BlockchainMetrics = BlockchainMetrics
  { systemStartTime        :: !UTCTime
  , systemStartTimeChanged :: !Bool
  , epoch                  :: !Integer
  , epochChanged           :: !Bool
  , epochLastUpdate        :: !Word64
  , slot                   :: !Integer
  , slotChanged            :: !Bool
  , slotLastUpdate         :: !Word64
  , blocksNumber           :: !Integer
  , blocksNumberChanged    :: !Bool
  , blocksNumberLastUpdate :: !Word64
  , chainDensity           :: !Double
  , chainDensityChanged    :: !Bool
  , chainDensityLastUpdate :: !Word64
  } deriving (Generic, NFData)

data KESMetrics = KESMetrics
  { remainingKESPeriods             :: !Integer
  , remainingKESPeriodsChanged      :: !Bool
  , remainingKESPeriodsInDays       :: !Integer
  , remainingKESPeriodsLastUpdate   :: !Word64
  , opCertStartKESPeriod            :: !Integer
  , opCertStartKESPeriodChanged     :: !Bool
  , opCertStartKESPeriodLastUpdate  :: !Word64
  , opCertExpiryKESPeriod           :: !Integer
  , opCertExpiryKESPeriodChanged    :: !Bool
  , opCertExpiryKESPeriodLastUpdate :: !Word64
  , currentKESPeriod                :: !Integer
  , currentKESPeriodChanged         :: !Bool
  , currentKESPeriodLastUpdate      :: !Word64
  } deriving (Generic, NFData)

data NodeMetrics = NodeMetrics
  { nodeProtocol         :: !Text
  , nodeProtocolChanged  :: !Bool
  , nodeVersion          :: !Text
  , nodeVersionChanged   :: !Bool
  , nodeCommit           :: !Text
  , nodeCommitChanged    :: !Bool
  , nodeShortCommit      :: !Text
  , nodePlatform         :: !Text
  , nodePlatformChanged  :: !Bool
  , nodeStartTime        :: !UTCTime
  , nodeStartTimeChanged :: !Bool
  , upTime               :: !Word64
  , upTimeLastUpdate     :: !Word64
  } deriving (Generic, NFData)

data ErrorsMetrics = ErrorsMetrics
  { errors        :: ![NodeError]
  , errorsChanged :: !Bool
  } deriving (Generic, NFData)

defaultNodesState
  :: Configuration
  -> IO NodesState
defaultNodesState config =
  CM.getAcceptAt config >>= \case
    Just addrs -> do
      return $ Map.fromList [(name, defaultNodeState) | (RemoteAddrNamed name _) <- addrs]
    Nothing ->
      -- Actually it's impossible, because at this point we already know
      -- that at least one |TraceAcceptor| is defined in the config.
      return Map.empty

defaultNodeState :: NodeState
defaultNodeState = NodeState
  { peersMetrics =
      PeerMetrics
        { peersInfo        = []
        , peersInfoChanged = True
        }
  , mempoolMetrics =
      MempoolMetrics
        { mempoolTxsNumber        = 0
        , mempoolTxsNumberChanged = True
        , mempoolTxsPercent       = 0.0
        , mempoolBytes            = 0
        , mempoolBytesChanged     = True
        , mempoolBytesPercent     = 0.0
        , mempoolMaxTxs           = 1
        , mempoolMaxBytes         = 1
        , txsProcessed            = 0
        , txsProcessedChanged     = True
        }
  , forgeMetrics =
      ForgeMetrics
        { nodeIsLeaderNum              = 0
        , nodeIsLeaderNumChanged       = True
        , nodeIsLeaderNumLastUpdate    = 0
        , slotsMissedNumber            = 0
        , slotsMissedNumberChanged     = True
        , slotsMissedNumberLastUpdate  = 0
        , nodeCannotForge              = 0
        , nodeCannotForgeChanged       = True
        , blocksForgedNumber           = 0
        , blocksForgedNumberChanged    = True
        , blocksForgedNumberLastUpdate = 0
        }
  , resourcesMetrics =
      ResourcesMetrics
        { memory                    = 0.0
        , memoryChanged             = True
        , memoryMax                 = 0.1
        , memoryMaxTotal            = 200.0
        , memoryPercent             = 0.0
        , memoryLastUpdate          = 0
        , cpuPercent                = 0.5
        , cpuPercentChanged         = True
        , cpuLast                   = 0
        , cpuNs                     = 10000
        , cpuLastUpdate             = 0
        , diskUsageR                = 0.0
        , diskUsageRChanged         = True
        , diskUsageRMax             = 0.0
        , diskUsageRMaxTotal        = 0.0
        , diskUsageRPercent         = 0.0
        , diskUsageRLast            = 0
        , diskUsageRNs              = 10000
        , diskUsageRAdaptTime       = UTCTime (ModifiedJulianDay 0) 0
        , diskUsageRLastUpdate      = 0
        , diskUsageW                = 0.0
        , diskUsageWChanged         = True
        , diskUsageWMax             = 0.0
        , diskUsageWMaxTotal        = 0.0
        , diskUsageWPercent         = 0.0
        , diskUsageWLast            = 0
        , diskUsageWNs              = 10000
        , diskUsageWAdaptTime       = UTCTime (ModifiedJulianDay 0) 0
        , diskUsageWLastUpdate      = 0
        , networkUsageIn            = 0.0
        , networkUsageInChanged     = True
        , networkUsageInPercent     = 0.0
        , networkUsageInMax         = 0.0
        , networkUsageInMaxTotal    = 0.0
        , networkUsageInLast        = 0
        , networkUsageInNs          = 10000
        , networkUsageInLastUpdate  = 0
        , networkUsageOut           = 0.0
        , networkUsageOutChanged    = True
        , networkUsageOutPercent    = 0.0
        , networkUsageOutMax        = 0.0
        , networkUsageOutMaxTotal   = 0.0
        , networkUsageOutLast       = 0
        , networkUsageOutNs         = 10000
        , networkUsageOutLastUpdate = 0
        }
  , rtsMetrics =
      RTSMetrics
        { rtsMemoryAllocated        = 1.0
        , rtsMemoryAllocatedChanged = True
        , rtsMemoryUsed             = 0.1
        , rtsMemoryUsedChanged      = True
        , rtsMemoryUsedPercent      = 1.0
        , rtsMemoryLastUpdate       = 0
        , rtsGcCpu                  = 0.1
        , rtsGcCpuChanged           = True
        , rtsGcCpuLastUpdate        = 0
        , rtsGcElapsed              = 0.1
        , rtsGcElapsedChanged       = True
        , rtsGcElapsedLastUpdate    = 0
        , rtsGcNum                  = 0
        , rtsGcNumChanged           = True
        , rtsGcNumLastUpdate        = 0
        , rtsGcMajorNum             = 0
        , rtsGcMajorNumChanged      = True
        , rtsGcMajorNumLastUpdate   = 0
        }
  , blockchainMetrics =
      BlockchainMetrics
        { systemStartTime        = UTCTime (ModifiedJulianDay 0) 0
        , systemStartTimeChanged = True
        , epoch                  = 0
        , epochChanged           = True
        , epochLastUpdate        = 0
        , slot                   = 0
        , slotChanged            = True
        , slotLastUpdate         = 0
        , blocksNumber           = 0
        , blocksNumberChanged    = True
        , blocksNumberLastUpdate = 0
        , chainDensity           = 0
        , chainDensityChanged    = True
        , chainDensityLastUpdate = 0
        }
  , kesMetrics =
      KESMetrics
        { remainingKESPeriods             = 9999999999
        , remainingKESPeriodsChanged      = True
        , remainingKESPeriodsInDays       = 9999999999
        , remainingKESPeriodsLastUpdate   = 0
        , opCertStartKESPeriod            = 9999999999
        , opCertStartKESPeriodChanged     = True
        , opCertStartKESPeriodLastUpdate  = 0
        , opCertExpiryKESPeriod           = 9999999999
        , opCertExpiryKESPeriodChanged    = True
        , opCertExpiryKESPeriodLastUpdate = 0
        , currentKESPeriod                = 9999999999
        , currentKESPeriodChanged         = True
        , currentKESPeriodLastUpdate      = 0
        }
  , nodeMetrics =
      NodeMetrics
        { nodeProtocol         = "-"
        , nodeProtocolChanged  = True
        , nodeVersion          = "-"
        , nodeVersionChanged   = True
        , nodeCommit           = "-"
        , nodeCommitChanged    = True
        , nodeShortCommit      = "-"
        , nodePlatform         = "-"
        , nodePlatformChanged  = True
        , nodeStartTime        = UTCTime (ModifiedJulianDay 0) 0
        , nodeStartTimeChanged = True
        , upTime               = 0
        , upTimeLastUpdate     = 0
        }
  , nodeErrors =
      ErrorsMetrics
        { errors        = []
        , errorsChanged = True
        }
  }
