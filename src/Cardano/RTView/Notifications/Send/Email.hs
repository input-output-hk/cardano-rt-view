{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.Notifications.Send.Email
    ( createAndSendEmails
    , createAndSendTestEmail
    ) where

import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Control.Monad.Extra (whenJust)
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
  void $ sendEmail (Just tr) eSettings email

createAndSendTestEmail
  :: EmailSettings
  -> IO Text
createAndSendTestEmail eSettings =
  sendEmail Nothing eSettings $ createTestEmail eSettings

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

createTestEmail
  :: EmailSettings
  -> Mail
createTestEmail EmailSettings {..} =
  simpleMail' to from emSubject body
 where
  to   = Address Nothing emEmailTo
  from = Address (Just "Cardano RTView") emEmailFrom
  body = "This is a test email from Cardano RTView. Congrats: your email notification settings are correct!"

sendEmail
  :: Maybe (Trace IO Text)
  -> EmailSettings
  -> Mail
  -> IO Text
sendEmail tr EmailSettings {..} mail =
  if cannotBeSent
    then logAndReturn logError cannotBeSentMessage
    else try (sender host port user pass mail) >>= \case
           Left (e :: SomeException) -> logAndReturn logError $ unableToSendMessage <> T.pack (show e)
           Right _ -> logAndReturn logNotice sentMessage
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
  cannotBeSentMessage = "Email cannot be sent: please fill in all inputs marked by an asterisk."
  unableToSendMessage = "Unable to send email: "
  sentMessage = "Yay! Email notification to " <> emEmailTo <> " sent."
  logAndReturn logger message = whenJust tr (flip logger message) >> return message
