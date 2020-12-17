module Cardano.RTView
    ( runCardanoRTView
    ) where

import           Control.Concurrent.Async (async, waitAnyCancel)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO

import           Cardano.BM.Backend.Switchboard (addUserDefinedBackend)
import           Cardano.BM.Data.Backend (Backend (..))
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace (Trace, logNotice)
import           Cardano.BM.Tracing (appendName)

import           Cardano.RTView.Acceptor (launchMetricsAcceptor)
import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.Config (prepareConfigAndParams)
import           Cardano.RTView.GUI.Elements (TmpElements, initialTmpElements)
import           Cardano.RTView.ErrorBuffer (ErrorBuffer, effectuate, realize, unrealize)
import           Cardano.RTView.NodeState.Types (NodesState, initialNodesState)
import           Cardano.RTView.NodeState.Updater (launchNodeStateUpdater)
import           Cardano.RTView.Notifications.CheckEvents (launchNotifications)
import           Cardano.RTView.Notifications.Types (NotificationSettings)
import           Cardano.RTView.WebServer (launchWebServer)

-- | Run the service.
runCardanoRTView :: RTViewParams -> IO ()
runCardanoRTView params' = do
  TIO.putStrLn "RTView: real-time watching for Cardano nodes"

  (config, notifySettings, params, acceptors) <- prepareConfigAndParams params'

  (tr :: Trace IO Text, switchBoard) <- Setup.setupTrace_ config "cardano-rt-view"
  let accTr = appendName "acceptor" tr

  -- Initialise own backend (error buffer).
  be :: ErrorBuffer Text <- realize config
  let ebBe = MkBackend { bEffectuate = effectuate be
                       , bUnrealize  = unrealize be
                       }
  addUserDefinedBackend switchBoard ebBe "ErrorBufferBK"

  logNotice tr "Starting service; hit CTRL-C to terminate..."

  -- This TVar contains state (info, metrics) for all nodes we receive metrics from.
  nsTVar :: TVar NodesState <- newTVarIO =<< initialNodesState config
  -- This TVar contains temporary Elements which should be deleted explicitly.
  tmpElsTVar :: TVar TmpElements <- newTVarIO initialTmpElements
  -- This TVar contains complete notification settings.
  notifyTVar :: TVar NotificationSettings <- newTVarIO notifySettings

  let nsTr = appendName "nodeState" tr
      wsTr = appendName "webServer" tr
      ntTr = appendName "notifications" tr
  -- Launch 4 threads:
  --   1. acceptor plugin (it launches |TraceAcceptor| plugin),
  --   2. node state updater (it gets metrics from |LogBuffer| and updates NodeState),
  --   3. web server (it serves requests from user's browser and shows nodes' metrics in the real time).
  --   4. notifications: check events and notify the user about them.
  acceptorThr <- async $ launchMetricsAcceptor config accTr switchBoard
  updaterThr  <- async $ launchNodeStateUpdater nsTr switchBoard be nsTVar
  serverThr   <- async $ launchWebServer wsTr config nsTVar tmpElsTVar notifyTVar params acceptors
  notifyThr   <- async $ launchNotifications ntTr nsTVar notifyTVar

  void $ waitAnyCancel [acceptorThr, updaterThr, serverThr, notifyThr]
