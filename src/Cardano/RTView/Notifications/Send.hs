module Cardano.RTView.Notifications.Send
    ( sendNotifications
    , sendTestEmail
    ) where

import           Data.Text (Text)

import           Cardano.BM.Trace (Trace)

import           Cardano.RTView.Notifications.Types
import           Cardano.RTView.Notifications.Send.Email (createAndSendEmails,
                                                          createAndSendTestEmail)

sendNotifications
  :: Trace IO Text
  -> NotificationSettings
  -> [NotifyEvent]
  -> IO ()
sendNotifications _ _ [] = return ()
sendNotifications tr settings notifyEvents = do
  -- Current release provides only email-notifications.
  let eSettings = emailSettings . nsHowToNotify $ settings
  createAndSendEmails tr eSettings notifyEvents

sendTestEmail
  :: NotificationSettings
  -> IO Text
sendTestEmail settings = do
  let eSettings = emailSettings . nsHowToNotify $ settings
  createAndSendTestEmail eSettings
