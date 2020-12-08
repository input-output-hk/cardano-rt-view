module Cardano.RTView.WebServer
    ( launchWebServer
    ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, liftIO, onEvent, set, (#), (#+))
import           Graphics.UI.Threepenny.Timer (interval, start, tick, timer)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))
import           Cardano.BM.Trace (Trace, appendName, logNotice)

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.GUI.CSS.Style (ownCSS)
import           Cardano.RTView.GUI.Elements (TmpElements, pageTitle)
import           Cardano.RTView.GUI.Markup.PageBody (mkPageBody)
import           Cardano.RTView.GUI.Updater (updateGUI)
import           Cardano.RTView.NodeState.Types (NodesState)

launchWebServer
  :: Trace IO Text
  -> Configuration
  -> TVar NodesState
  -> TVar TmpElements
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> IO ()
launchWebServer tr config nsTVar tmpElsTVar params acceptors =
  UI.startGUI wsConfig $ mainPage tr config nsTVar tmpElsTVar params acceptors
 where
  wsConfig = UI.defaultConfig
    { UI.jsStatic = Just $ rtvStatic params
    , UI.jsPort   = Just $ rtvPort params
    -- By default it listens on 127.0.0.1, but it cannot be accessed
    -- from another machine, so change it to 0.0.0.0.
    , UI.jsAddr   = Just "0.0.0.0"
    }

mainPage
  :: Trace IO Text
  -> Configuration
  -> TVar NodesState
  -> TVar TmpElements
  -> RTViewParams
  -> [RemoteAddrNamed]
  -> UI.Window
  -> UI ()
mainPage tr config nsTVar tmpElsTVar params acceptors window = do
  liftIO $ logNotice tr "Web page loading..."

  void $ return window # set UI.title pageTitle

  -- It is assumed that CSS files are available at 'pathToStatic/css/'.
  UI.addStyleSheet window "w3.css"
  embedOwnCSS window

  -- It is assumed that JS files are available at 'pathToStatic/js/'.
  addJavaScript window "chart.js"

  -- Make page's body (HTML markup).
  (pageBody, nodesStateElems) <- mkPageBody config nsTVar tmpElsTVar params window acceptors

  let guiTr = appendName "GUI" tr
  -- Start the timer for GUI update. Every second it will
  -- call a function which updates node state elements on the page.
  guiUpdateTimer <- timer # set interval 1000
  void $ onEvent (tick guiUpdateTimer) $ \_ ->
    updateGUI guiTr window nsTVar tmpElsTVar params nodesStateElems
  start guiUpdateTimer

  void $ UI.element pageBody

-- | Add JS library stored locally.
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
