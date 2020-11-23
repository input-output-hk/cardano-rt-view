{-# LANGUAGE LambdaCase #-}

module Cardano.RTView.GUI.Markup.PageBody
    ( mkPageBody
    ) where

import           Control.Concurrent.STM.TVar (TVar, modifyTVar')
import           Control.Monad (forM, forM_, void, when)
import           Control.Monad.Extra (whenJustM)
import           Control.Monad.STM (atomically)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, (#), (#+))

import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))
import qualified Cardano.RTView.GUI.JS.Charts as Chart
import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), NodesStateElements,
                                              hideIt, showCell, showIt, showRow, (##), (#.))
import           Cardano.RTView.GUI.Markup.Grid (allMetricsNames, metricLabel, mkNodesGrid)
import           Cardano.RTView.GUI.Markup.Pane (mkNodesPanes)
import           Cardano.RTView.NodeState.Types

mkPageBody
  :: TVar NodesState
  -> UI.Window
  -> [RemoteAddrNamed]
  -> UI (Element, (NodesStateElements, NodesStateElements))
mkPageBody nsTVar window acceptors = do
  (paneNodesRootElem, paneNodesElems, panesWithNames) <- mkNodesPanes nsTVar acceptors
  (gridNodesRootElem, gridNodesElems) <- mkNodesGrid nsTVar acceptors

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
      whenJustM (UI.getElementById window (show ViewModeButton)) $ \btn ->
        UI.get UI.value btn >>= \case
          "paneMode" -> do
            let action = if isChecked then showIt else hideIt
            forNodePane nameOfNode panesWithNames action
          _ -> do
            let action = if isChecked then showCell else hideIt
            forNodeColumn window nameOfNode action
      changeStatusOfShowAllButton window ShowAllNodesButton SelectNodeCheck
    return $ element nodeButton

  showAndHideAllNodesButtons
    <- if length nodesSelector > 1
         then do
           showAllNodesButton <- showAllButton ShowAllNodesButton
           hideAllNodesButton <- hideAllButton HideAllNodesButton

           void $ UI.onEvent (UI.click showAllNodesButton) $ \_ -> do
             whenJustM (UI.getElementById window (show ViewModeButton)) $ \btn ->
               UI.get UI.value btn >>= \case
                 "paneMode" -> showAllNodes window panesWithNames
                 _ -> showAllNodesColumns window panesWithNames
             void $ element showAllNodesButton #. [W3BarItem, W3Button, W3Disabled]
             void $ element hideAllNodesButton #. [W3BarItem, W3Button, W3BorderBottom]

           void $ UI.onEvent (UI.click hideAllNodesButton) $ \_ -> do
             whenJustM (UI.getElementById window (show ViewModeButton)) $ \btn ->
               UI.get UI.value btn >>= \case
                 "paneMode" -> hideAllNodes window panesWithNames
                 _ -> hideAllNodesColumns window panesWithNames
             void $ element showAllNodesButton #. [W3BarItem, W3Button]
             void $ element hideAllNodesButton #. [W3BarItem, W3Button, W3BorderBottom, W3Disabled]

           return [element showAllNodesButton, element hideAllNodesButton]
         else
           return []

  let allSelectors = showAndHideAllNodesButtons ++ nodesSelector

  -- View mode buttons.
  paneViewButton <- UI.anchor #. [W3BarItem, W3Button] # set UI.href "#" #+ [UI.string "Pane view"]
  gridViewButton <- UI.anchor #. [W3BarItem, W3Button] # set UI.href "#" #+ [UI.string "Grid view"]
  let viewModeSelector :: [UI Element]
      viewModeSelector = [ element paneViewButton
                         , element gridViewButton
                         ]

  metricsSelector <- mkMetricsSelector window

  bodyRootElem <- UI.div #+ [element paneNodesRootElem] -- Pane view mode is the default now.

  body
    <- UI.getBody window #+
         [ topNavigation allSelectors viewModeSelector metricsSelector
         , element bodyRootElem
         ]

  UI.runFunction $ UI.ffi Chart.prepareChartsJS

  void $ UI.onEvent (UI.click paneViewButton) $ \_ -> do
    toggleViewMode window "paneMode" bodyRootElem [element paneNodesRootElem] [element gridNodesRootElem]
    forceChangingAllElements nsTVar
    forElementWithId window (show SelectMetricButton) hideIt
  void $ UI.onEvent (UI.click gridViewButton) $ \_ -> do
    toggleViewMode window "gridMode" bodyRootElem [element gridNodesRootElem] [element paneNodesRootElem]
    forceChangingAllElements nsTVar
    forElementWithId window (show SelectMetricButton) showIt
    forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
      UI.runFunction $ UI.ffi Chart.gridMemoryUsageChartJS  (showt GridMemoryUsageChartId  <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridCPUUsageChartJS     (showt GridCPUUsageChartId     <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridDiskUsageChartJS    (showt GridDiskUsageChartId    <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridNetworkUsageChartJS (showt GridNetworkUsageChartId <> nameOfNode)

  forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    -- Charts for different metrics.
    UI.runFunction $ UI.ffi Chart.memoryUsageChartJS  (showt MemoryUsageChartId  <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.cpuUsageChartJS     (showt CPUUsageChartId     <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.diskUsageChartJS    (showt DiskUsageChartId    <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.networkUsageChartJS (showt NetworkUsageChartId <> nameOfNode)

  return (body, (paneNodesElems, gridNodesElems))
 where
  showt :: Show a => a -> Text
  showt = T.pack . show

forceChangingAllElements :: TVar NodesState -> UI ()
forceChangingAllElements nsTVar =
  liftIO . atomically $ modifyTVar' nsTVar $ \currentNS ->
    let updatedNS = map (\(nm, ns) -> (nm, setAllChangedFlags ns)) $ HM.toList currentNS
    in HM.fromList updatedNS
 where
   setAllChangedFlags ns = ns

topNavigation
  :: [UI Element]
  -> [UI Element]
  -> [UI Element]
  -> UI Element
topNavigation nodesSelector viewModeSelector metricsSelector =
  UI.div #. [W3Bar, W3Large, TopBar] #+
    [ UI.anchor #. [W3BarItem, W3Mobile] # set UI.href "https://cardano.org/" #+
        [ UI.img #. [CardanoLogo] # set UI.src "/static/images/cardano-logo.svg"
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button ## show ViewModeButton
                    #. [W3Button]
                    # set UI.value "paneMode"
                    #+ [ string "View mode"
                       , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
                       ]
        , UI.div #. [W3DropdownContent, W3BarBlock] #+ viewModeSelector
        ]
    , UI.div #. [W3DropdownHover, W3Mobile] #+
        [ UI.button #. [W3Button] #+
            [ string "Select node"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock] #+ nodesSelector
        ]
    , UI.div ## show SelectMetricButton #. [W3DropdownHover, W3Mobile] # hideIt #+
        [ UI.button #. [W3Button] #+
            [ string "Select metric"
            , UI.img #. [TopNavDropdownIcon] # set UI.src "/static/images/dropdown-light.svg"
            ]
        , UI.div #. [W3DropdownContent, W3BarBlock, MetricsArea] #+ metricsSelector
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

forNodeColumn
  :: UI.Window
  -> Text
  -> (UI Element -> UI Element)
  -> UI ()
forNodeColumn window nameOfNode action = do
  let cellsIdsForNodeColumn =
        map (\elemName -> show elemName <> "-" <> T.unpack nameOfNode)
            allMetricsNames
  let allCells = (show GridNodeTH <> T.unpack nameOfNode) : cellsIdsForNodeColumn
  forM_ allCells $ \anId ->
    forElementWithId window anId action

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

showAllNodesColumns, hideAllNodesColumns
  :: UI.Window
  -> [(Text, Element)]
  -> UI ()
showAllNodesColumns = changeNodesColumnsVisibility True
hideAllNodesColumns = changeNodesColumnsVisibility False

changeNodesColumnsVisibility
  :: Bool
  -> UI.Window
  -> [(Text, Element)]
  -> UI ()
changeNodesColumnsVisibility showThem window panesWithNames' = do
  forM_ panesWithNames' $ \(nameOfNode, _) ->
    forNodeColumn window nameOfNode $ if showThem then showCell else hideIt
  nodesCheckboxes <- UI.getElementsByClassName window (show SelectNodeCheck)
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked showThem

toggleViewMode
  :: UI.Window
  -> String
  -> Element
  -> [UI Element]
  -> [UI Element]
  -> UI ()
toggleViewMode window newValue rootElem childrenToAdd childrenToDelete = do
  -- Store current view mode in the view mode button.
  forElementWithId window (show ViewModeButton) (set UI.value newValue)
  -- Delete these elements from DOM.
  mapM_ (fmap UI.delete) childrenToDelete
  -- Explicitly remove current children of rootElem and set the new ones.
  void $ element rootElem # set UI.children []
  void $ element rootElem #+ childrenToAdd

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

mkMetricsSelector
  :: UI.Window
  -> UI [UI Element]
mkMetricsSelector window = do
  showAllMetricsButton <- showAllButton ShowAllMetricsButton
  hideAllMetricsButton <- hideAllButton HideAllMetricsButton

  void $ UI.onEvent (UI.click showAllMetricsButton) $ \_ -> do
    showAllMetrics window allMetricsNames
    void $ element showAllMetricsButton #. [W3BarItem, W3Button, W3Disabled]
    void $ element hideAllMetricsButton #. [W3BarItem, W3Button, W3BorderBottom]

  void $ UI.onEvent (UI.click hideAllMetricsButton) $ \_ -> do
    hideAllMetrics window allMetricsNames
    void $ element showAllMetricsButton #. [W3BarItem, W3Button]
    void $ element hideAllMetricsButton #. [W3BarItem, W3Button, W3BorderBottom, W3Disabled]

  checkboxes <-
    forM allMetricsNames $ \aName ->
      element <$> mkCheckbox window aName

  return $    [element showAllMetricsButton]
           ++ [element hideAllMetricsButton]
           ++ checkboxes

mkCheckbox
  :: UI.Window
  -> ElementName
  -> UI Element
mkCheckbox window elemName = do
  metricCheckbox
    <- UI.input #. [W3Check, SelectMetricCheck]
                # set UI.type_ "checkbox"
                # set UI.checked True
                #+ []
  void $ UI.onEvent (UI.checkedChange metricCheckbox) $ \isChecked -> do
    let action = if isChecked then showRow else hideIt
    forElementWithId window (show elemName) action
    changeStatusOfShowAllButton window ShowAllMetricsButton SelectMetricCheck
    changeStatusOfHideAllButton window HideAllMetricsButton SelectMetricCheck

  UI.div #. [SelectMetricCheckArea] #+
    [ element metricCheckbox
    , UI.label #+ [UI.string $ fst $ metricLabel elemName]
    ]

forElementWithId
  :: UI.Window
  -> String
  -> (UI Element -> UI Element)
  -> UI ()
forElementWithId window anId action =
  whenJustM (UI.getElementById window anId) $ \el ->
    void $ element el # action

showAllMetrics, hideAllMetrics
  :: UI.Window
  -> [ElementName]
  -> UI ()
showAllMetrics = changeMetricsVisibility True
hideAllMetrics = changeMetricsVisibility False

changeMetricsVisibility
  :: Bool
  -> UI.Window
  -> [ElementName]
  -> UI ()
changeMetricsVisibility showThem window metricsElems = do
  forM_ metricsElems $ \elemName ->
    forElementWithId window (show elemName) (if showThem then showRow else hideIt)
  metricsCheckboxes <- UI.getElementsByClassName window (show SelectMetricCheck)
  forM_ metricsCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked showThem

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
