{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.Server
    ( launchServer
    ) where

import           Cardano.Prelude hiding (readMVar)

import           Control.Concurrent.MVar.Strict (MVar, readMVar)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, onEvent, set, ( # ), ( #+ ))
import           Graphics.UI.Threepenny.Timer (interval, start, tick, timer)

import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.GUI.CSS.Style (ownCSS)
import           Cardano.RTView.GUI.Markup (mkPageBody)
import           Cardano.RTView.GUI.Updater (updateGUI)
import           Cardano.RTView.NodeState.Types (NodesState)

-- | Launch web server.
launchServer
  :: MVar NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> IO ()
launchServer nsMVar params acceptors =
  UI.startGUI config $ mainPage nsMVar params acceptors
 where
  config = UI.defaultConfig
    { UI.jsStatic = Just $ rtvStatic params
    , UI.jsPort   = Just $ rtvPort params
    }

mainPage
  :: MVar NodesState
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> UI.Window
  -> UI ()
mainPage nsMVar params acceptors window = do
  void $ return window # set UI.title "Cardano Node RTView"

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  embedOwnCSS window

  -- It is assumed that JS files are available at 'pathToStatic/js/'.
  addJavaScript window "chart.js"

  -- Make page's body (HTML markup).
  (pageBody, (nodesStateElems, gridNodesStateElems)) <- mkPageBody window acceptors

  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 2000 -- Every 2 s.
  void $ onEvent (tick guiUpdateTimer) $ \_ -> do
    newState <- liftIO $ readMVar nsMVar
    updateGUI window newState params acceptors (nodesStateElems, gridNodesStateElems)
  start guiUpdateTimer

  void $ UI.element pageBody

-- | ...
addJavaScript
  :: UI.Window
  -> FilePath
  -> UI ()
addJavaScript w filename = void $ do
  el <- UI.mkElement "script" # set UI.src ("/static/js/" ++ filename)
  UI.getHead w #+ [UI.element el]

-- | We generate our own CSS using 'clay' package, so embed it in the page's header.
embedOwnCSS
  :: UI.Window
  -> UI ()
embedOwnCSS w = void $ do
  el <- UI.mkElement "style" # set UI.html ownCSS
  UI.getHead w #+ [UI.element el]
