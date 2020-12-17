{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.Notifications.Send.Email
    ( createAndSendEmails
    ) where

import           Control.Exception (IOException, try)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           Network.Mail.SMTP (sendMailWithLogin', sendMailWithLoginSTARTTLS',
                                    sendMailWithLoginTLS')
import           Network.Mail.Mime (Address (..), Mail (..), simpleMail')

import           Cardano.BM.Trace (Trace, logError, logDebug, logNotice)

import           Cardano.RTView.Notifications.Types

createAndSendEmails
  :: Trace IO Text
  -> EmailSettings
  -> [NotifyEvent]
  -> IO ()
createAndSendEmails _ _ [] = return ()
createAndSendEmails tr eSettings events = do
  -- To keep email traffic as low as possible,
  -- we create only one email with all events in it.
  let email = createEmail eSettings events
  logDebug tr $ "Email notifications, new email: " <> T.pack (show email)
  sendEmail tr eSettings email

createEmail
  :: EmailSettings
  -> [NotifyEvent]
  -> Mail
createEmail EmailSettings {..} events =
  simpleMail' to from emSubject body
 where
  to     = Address Nothing emEmailTo
  from   = Address (Just "Cardano RTView") emEmailFrom
  body   = LT.fromStrict . T.intercalate "\n\n" $ header : map mkBodyPart events
  header = "Cardano RTView Notification"
  mkBodyPart NotifyEvent {..} =
    T.pack (show evTime) <> ", from the node '" <> evNodeName <> "': " <> evMessage

sendEmail
  :: Trace IO Text
  -> EmailSettings
  -> Mail
  -> IO ()
sendEmail tr eSet@EmailSettings {..} mail =
  if cannotBeSent
    then logError tr $ "Email cannot be sent because of lack of required email settings: "
                       <> T.pack (show eSet)
    else do
      try (sender host port user pass mail) >>= \case
        Left (e :: IOException) -> logError tr $ "Unable to send email: " <> T.pack (show e)
        Right _ -> logNotice tr $ "Email notification to " <> emEmailTo <> " sent."
 where
  sender = case emSSL of
             TLS      -> sendMailWithLoginTLS'
             StartTLS -> sendMailWithLoginSTARTTLS'
             NoSSL    -> sendMailWithLogin'
  host = T.unpack emServerHost
  port = fromIntegral emServerPort
  user = T.unpack emUsername
  pass = T.unpack emPassword
  cannotBeSent =    null host
                 || port == 0
                 || null user
                 || null pass
                 || T.null emEmailTo
