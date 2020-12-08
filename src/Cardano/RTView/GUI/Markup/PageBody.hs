{-# LANGUAGE LambdaCase #-}

module Cardano.RTView.GUI.Markup.PageBody
    ( mkPageBody
    ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad (forM, forM_, void, when)
import           Control.Monad.Extra (whenJustM)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, set, string, (#), (#+))

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

import           Cardano.RTView.CLI (RTViewParams (..))
import qualified Cardano.RTView.GUI.JS.Charts as Chart
import           Cardano.RTView.GUI.Elements (HTMLClass (..), HTMLId (..),
                                              NodesStateElements, TmpElements,
                                              hideIt, showIt, (##), (#.))
import           Cardano.RTView.GUI.Markup.OwnInfo (mkOwnInfo)
import           Cardano.RTView.GUI.Markup.Pane (mkNodesPanes)
import           Cardano.RTView.NodeState.Types

mkPageBody
  :: Configuration
  -> TVar NodesState
  -> TVar TmpElements
  -> RTViewParams
  -> UI.Window
  -> [RemoteAddrNamed]
  -> UI (Element, NodesStateElements)
mkPageBody config nsTVar tmpElsTVar params window acceptors = do
  (paneNodesRootElem, paneNodesElems, panesWithNames) <- mkNodesPanes nsTVar tmpElsTVar acceptors

  -- Register clickable selector for nodes (to be able to show only one or all of them).
  nodesSelector <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    nodeCheckbox
      <- UI.input #. [W3Check, SelectNodeCheck]
                  # set UI.type_ "checkbox"
                  # set UI.checked True
                  #+ []
    nodeButton <-
      UI.div #. [SelectNodeCheckArea] #+
        [ element nodeCheckbox
        , UI.label #+ [UI.string $ T.unpack nameOfNode]
        ]

    void $ UI.onEvent (UI.checkedChange nodeCheckbox) $ \isChecked -> do
      let action = if isChecked then showIt else hideIt
      forNodePane nameOfNode panesWithNames action
      changeStatusOfShowAllButton window ShowAllNodesButton SelectNodeCheck
      changeStatusOfHideAllButton window HideAllNodesButton SelectNodeCheck

    return $ element nodeButton

  showAndHideAllNodesButtons
    <- if length nodesSelector > 1
         then do
           showAllNodesButton <- showAllButton ShowAllNodesButton
           hideAllNodesButton <- hideAllButton HideAllNodesButton

           void $ UI.onEvent (UI.click showAllNodesButton) $ \_ -> do
             showAllNodes window panesWithNames
             void $ element showAllNodesButton #. [W3BarItem, W3Button, W3Disabled]
             void $ element hideAllNodesButton #. [W3BarItem, W3Button, W3BorderBottom]

           void $ UI.onEvent (UI.click hideAllNodesButton) $ \_ -> do
             hideAllNodes window panesWithNames
             void $ element showAllNodesButton #. [W3BarItem, W3Button]
             void $ element hideAllNodesButton #. [W3BarItem, W3Button, W3BorderBottom, W3Disabled]

           return [element showAllNodesButton, element hideAllNodesButton]
         else
           return []

  let allNodesSelectors = showAndHideAllNodesButtons ++ nodesSelector

  body
    <- UI.getBody window #+
         [ topNavigation config params allNodesSelectors
         , element paneNodesRootElem
         ]

  UI.runFunction $ UI.ffi Chart.prepareChartsJS

  forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    -- Charts for different metrics.
    UI.runFunction $ UI.ffi Chart.memoryUsageChartJS  (showt MemoryUsageChartId  <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.cpuUsageChartJS     (showt CPUUsageChartId     <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.diskUsageChartJS    (showt DiskUsageChartId    <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.networkUsageChartJS (showt NetworkUsageChartId <> nameOfNode)

  return (body, paneNodesElems)
 where
  showt :: Show a => a -> Text
  showt = T.pack . show

topNavigation
  :: Configuration
  -> RTViewParams
  -> [UI Element]
  -> UI Element
topNavigation config params nodesSelector = do
  rtViewInfo <- mkOwnInfo config params
  rtViewInfoButton <- UI.img #. [RTViewInfoIcon]
                             # set UI.src "/static/images/info-light.svg"
                             # set UI.title__ "RTView info"
  void $ UI.onEvent (UI.click rtViewInfoButton) $ \_ -> do
    element rtViewInfo # showIt

  UI.div #. [W3Bar, W3Large, TopBar] #+
    [ UI.anchor #. [W3BarItem, W3Mobile] # set UI.href "https://cardano.org/" #+
        [ UI.img #. [CardanoLogo] # set UI.src "/static/images/cardano-logo.svg"
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Select node"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock] #+ nodesSelector
        ]
    , UI.span #. [W3BarItem, W3Mobile] #+
        [ element rtViewInfoButton
        , element rtViewInfo
        ]
    , UI.span #. [W3Right, W3HideMedium, W3HideSmall, ServiceName] #+
        [ string "Cardano Node Real-time View"
        ]
    ]

forNodePane
  :: Text
  -> [(Text, Element)]
  -> (UI Element -> UI Element)
  -> UI ()
forNodePane nameOfNode panesWithNames' action =
  forM_ panesWithNames' $ \(aName, pane) ->
    when (aName == nameOfNode) $
      void $ element pane # action

showAllNodes, hideAllNodes
  :: UI.Window
  -> [(Text, Element)]
  -> UI ()
showAllNodes = changeNodesVisibility True
hideAllNodes = changeNodesVisibility False

changeNodesVisibility
  :: Bool
  -> UI.Window
  -> [(Text, Element)]
  -> UI ()
changeNodesVisibility showThem window panesWithNames' = do
  forM_ panesWithNames' $ \(_, pane) ->
    void $ element pane # if showThem then showIt else hideIt
  nodesCheckboxes <- UI.getElementsByClassName window (show SelectNodeCheck)
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked showThem

-- | If all checkboxes are checked - "Show all" button should be disabled.
--   If at least one of them are unchecked - "Show all" button should be enabled.
changeStatusOfShowAllButton
  :: UI.Window
  -> HTMLId
  -> HTMLClass
  -> UI ()
changeStatusOfShowAllButton window anId aClass =
  whenJustM (UI.getElementById window (show anId)) $ \button -> do
    checkboxes <- UI.getElementsByClassName window (show aClass)
    statuses <- mapM (UI.get UI.checked) checkboxes
    if all (True ==) statuses
      then void $ element button #. [W3BarItem, W3Button, W3Disabled]
      else void $ element button #. [W3BarItem, W3Button]

-- | If all checkboxes are unchecked - "Hide all" button should be disabled.
--   If at least one of them are checked - "Hide all" button should be enabled.
changeStatusOfHideAllButton
  :: UI.Window
  -> HTMLId
  -> HTMLClass
  -> UI ()
changeStatusOfHideAllButton window anId aClass =
  whenJustM (UI.getElementById window (show anId)) $ \button -> do
    checkboxes <- UI.getElementsByClassName window (show aClass)
    statuses <- mapM (UI.get UI.checked) checkboxes
    if all (False ==) statuses
      then void $ element button #. [W3BarItem, W3Button, W3BorderBottom, W3Disabled]
      else void $ element button #. [W3BarItem, W3Button, W3BorderBottom]

showAllButton, hideAllButton :: HTMLId -> UI Element
showAllButton anId = mkButton anId [W3BarItem, W3Button, W3Disabled]     "show.svg" "Show all"
hideAllButton anId = mkButton anId [W3BarItem, W3Button, W3BorderBottom] "hide.svg" "Hide all"

mkButton
  :: HTMLId
  -> [HTMLClass]
  -> String
  -> String
  -> UI Element
mkButton anId classes icon label =
  UI.anchor ## show anId #. classes # set UI.href "#" #+
    [ UI.img #. [ShowHideIcon] # set UI.src ("/static/images/" <> icon)
    , string label
    ]
