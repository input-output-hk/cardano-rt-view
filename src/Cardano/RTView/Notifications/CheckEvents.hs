{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.Notifications.CheckEvents
    ( launchNotifications
    ) where

import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad (forM, forever, when)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import           Data.Time.Clock (getCurrentTime)
import           System.Time.Extra (sleep)

import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Trace (Trace, logDebug)

import           Cardano.RTView.NodeState.Types
import           Cardano.RTView.Notifications.Types
import           Cardano.RTView.Notifications.Send (sendNotifications)

launchNotifications
  :: Trace IO Text
  -> TVar NodesState
  -> TVar NotificationSettings
  -> IO ()
launchNotifications tr nsTVar notifyTVar = forever $ do
  nSet@NotificationSettings {..} <- readTVarIO notifyTVar
  logDebug tr $ "Notifications enabled: " <> pack (show nsEnabled)
  when nsEnabled $ do
    logDebug tr $ "Notifications are enabled, current settings: " <> pack (show nSet)
    notifyEvents <- checkNotifyEvents nsTVar nSet
    sendNotifications tr nSet notifyEvents
  sleep $ fromIntegral nsCheckPeriodInSec

checkNotifyEvents
  :: TVar NodesState
  -> NotificationSettings
  -> IO [NotifyEvent]
checkNotifyEvents nsTVar settings = do
  let EventsToNotify {..} = nsEventsToNotify settings
  currentNodesState <- readTVarIO nsTVar
  errs  <- checkErrors errorsEvents currentNodesState
  bErrs <- checkBlockchainErrors blockchainEvents currentNodesState
  return $ errs ++ bErrs

checkErrors
  :: ErrorsEvents
  -> NodesState
  -> IO [NotifyEvent]
checkErrors ErrorsEvents {..} nodesState =
  concat <$> forM (HM.toList nodesState) mkErrEventsForNode
 where
  mkErrEventsForNode (nameOfNode, nodeState) = do
    let ErrorsMetrics {..} = nodeErrors nodeState
        errorsShouldBeNotified = filter errorShouldBeNotified errors
    return $ map (\(NodeError ts sev msg _) -> NotifyEvent ts nameOfNode (mkErrMessage sev msg))
                 errorsShouldBeNotified

  errorShouldBeNotified (NodeError _ sev _ _) =
    case sev of
      Warning   -> aboutWarnings
      Error     -> aboutErrors
      Critical  -> aboutCriticals
      Alert     -> aboutAlerts
      Emergency -> aboutEmergencies
      _         -> False

  mkErrMessage sev msg =
    "Error occurred: severity '" <> pack (show sev) <> "', message: " <> msg

checkBlockchainErrors
  :: BlockchainEvents
  -> NodesState
  -> IO [NotifyEvent]
checkBlockchainErrors BlockchainEvents {..} nodesState =
  concat <$> forM (HM.toList nodesState) mkBCEventsForNode
 where
  mkBCEventsForNode (nameOfNode, nodeState) = do
    let ForgeMetrics {..} = forgeMetrics nodeState
    ts <- getCurrentTime
    let slotsMissedEv =
          if slotsMissedNumber > 0 && aboutMissedSlots
            then [NotifyEvent ts nameOfNode $ pack (show slotsMissedNumber) <> " slots were missed!"]
            else []
        cannotForgeEv =
          if nodeCannotForge > 0 && aboutCannotForge
            then [NotifyEvent ts nameOfNode $ "Node couldn't forge " <> pack (show aboutCannotForge) <> " times!"]
            else []
    return $ slotsMissedEv ++ cannotForgeEv
