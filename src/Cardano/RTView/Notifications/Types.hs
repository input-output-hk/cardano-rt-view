{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.RTView.Notifications.Types
    ( NotifyEvent (..)
    , NotificationSettings (..)
    , ErrorsEvents (..)
    , BlockchainEvents (..)
    , EventsToNotify (..)
    , HowToNotify (..)
    , SSL (..)
    , EmailSettings (..)
    , initialNotificationSettings
    ) where

import           Control.DeepSeq (NFData (..))
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime (..))
import           GHC.Generics (Generic)

-- | Some event we should notify about.
data NotifyEvent = NotifyEvent
  { evTime     :: !UTCTime
  , evNodeName :: !Text
  , evMessage  :: !Text
  } deriving (Eq, Generic, NFData, Show)

-- | Complete notification settings.
data NotificationSettings = NotificationSettings
  { nsEnabled          :: !Bool
  , nsCheckPeriodInSec :: !Int
  , nsEventsToNotify   :: !EventsToNotify
  , nsHowToNotify      :: !HowToNotify
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data ErrorsEvents = ErrorsEvents
  { aboutWarnings    :: !Bool
  , aboutErrors      :: !Bool
  , aboutCriticals   :: !Bool
  , aboutAlerts      :: !Bool
  , aboutEmergencies :: !Bool
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data BlockchainEvents = BlockchainEvents
  { aboutMissedSlots :: !Bool
  , aboutCannotForge :: !Bool
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data EventsToNotify = EventsToNotify
  { errorsEvents     :: !ErrorsEvents
  , blockchainEvents :: !BlockchainEvents
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data HowToNotify = HowToNotify
  { emailSettings :: !EmailSettings
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data SSL
  = TLS
  | StartTLS
  | NoSSL
  deriving (Eq, Generic, NFData, Show, Read, FromJSON, ToJSON)

data EmailSettings = EmailSettings
  { emServerHost :: !Text
  , emServerPort :: !Int
  , emUsername   :: !Text
  , emPassword   :: !Text
  , emSSL        :: !SSL
  , emEmailFrom  :: !Text
  , emEmailTo    :: !Text
  , emSubject    :: !Text
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

initialNotificationSettings :: NotificationSettings
initialNotificationSettings = NotificationSettings
  { nsEnabled = True
  , nsCheckPeriodInSec = 120
  , nsEventsToNotify =
      EventsToNotify
        { errorsEvents =
            ErrorsEvents
              { aboutWarnings    = True
              , aboutErrors      = True
              , aboutCriticals   = True
              , aboutAlerts      = True
              , aboutEmergencies = True
              }
        , blockchainEvents =
            BlockchainEvents
              { aboutMissedSlots = True
              , aboutCannotForge = True
              }
        }
  , nsHowToNotify =
      HowToNotify
        { emailSettings =
            EmailSettings
              { emServerHost = ""
              , emServerPort = 587
              , emUsername   = ""
              , emPassword   = ""
              , emSSL        = TLS
              , emEmailFrom  = ""
              , emEmailTo    = ""
              , emSubject    = "Cardano RTView Notification"
              }
        }
  }
