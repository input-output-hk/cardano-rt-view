{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.GUI.Markup
    ( mkPageBody
    ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, set, string, ( # ), ( #+ ),
                                              ( #. ))

import qualified Cardano.RTView.GUI.Charts as Chart
import           Cardano.RTView.GUI.Elements (ElementName (..), HTMLClass (..),
                                              HTMLId (..), HTMLW3Class (..),
                                              NodeStateElements, NodesStateElements,
                                              PeerInfoItem, hideIt, showCell, showIt,
                                              showRow, ( ## ), (<+>))
import           Cardano.RTView.GUI.Grid (allMetricsNames, metricLabel, mkNodesGrid)
import           Cardano.RTView.GUI.Pane (mkNodePane)
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

mkPageBody
  :: UI.Window
  -> [RemoteAddrNamed]
  -> UI ( Element
        , (NodesStateElements, NodesStateElements)
        )
mkPageBody window acceptors = do
  -- Create panes for each node (corresponding to acceptors).
  nodePanesWithElems
    <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
         (pane, nodeStateElems, peerInfoItems) <- mkNodePane nameOfNode
         return (nameOfNode, pane, nodeStateElems, peerInfoItems)

  -- Create panes areas on the page.
  panesAreas
    <- forM nodePanesWithElems $ \(_, pane, _, _) ->
         return $ UI.div #. [W3Col, W3L6, W3M12, W3S12] <+> [] #+ [element pane]

  -- Register clickable selector for nodes (to be able to show only one or all of them).
  nodesSelector <- forM acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    nodeCheckbox
      <- UI.input #. [W3Check] <+> [SelectNodeCheck]
                  # set UI.type_ "checkbox"
                  # set UI.checked True
                  #+ []
    nodeButton <-
      UI.div #. show SelectNodeCheckArea #+
        [ element nodeCheckbox
        , UI.label #+ [UI.string $ T.unpack nameOfNode]
        ]
    void $ UI.onEvent (UI.checkedChange nodeCheckbox) $ \isChecked -> do
      UI.getElementById window (show ViewModeButton) >>= \case
        Just btn -> UI.get UI.value btn >>= \case
          "paneMode" -> do
            let action = if isChecked then showIt else hideIt
            forNode nameOfNode nodePanesWithElems action
          _ -> do
            let action = if isChecked then showCell else hideIt
            forNodeColumn window nameOfNode action

        Nothing -> return ()
      changeStatusOfShowAllButton window ShowAllNodesButton SelectNodeCheck
    return $ element nodeButton

  showAllNodesButton
    <- if length nodesSelector > 1
         then do
           allNodesButton <- UI.anchor ## show ShowAllNodesButton
                                       #. [W3BarItem, W3Button, W3BorderTop, W3Disabled] <+> []
                                       # set UI.href "#"
                                       #+ [UI.string "Show all"]
           void $ UI.onEvent (UI.click allNodesButton) $ \_ -> do
             UI.getElementById window (show ViewModeButton) >>= \case
               Just btn -> UI.get UI.value btn >>= \case
                 "paneMode" -> showAllNodes window nodePanesWithElems
                 _ -> showAllNodesColumns window nodePanesWithElems
               Nothing -> return ()
             -- All nodes checkboxes are already shown, disable button again.
             void $ element allNodesButton # set UI.class_ ([W3BarItem, W3Button, W3BorderTop, W3Disabled] <+> [])
           return [element allNodesButton]
         else
           return []

  let allSelectors = nodesSelector ++ showAllNodesButton

  -- View mode buttons.
  paneViewButton <- UI.anchor #. [W3BarItem, W3Button] <+> [] # set UI.href "#" #+ [UI.string "Pane view"]
  gridViewButton <- UI.anchor #. [W3BarItem, W3Button] <+> [] # set UI.href "#" #+ [UI.string "Grid view"]
  let viewModeSelector :: [UI Element]
      viewModeSelector = [ element paneViewButton
                         , element gridViewButton
                         ]

  metricsSelector <- mkMetricsSelector window

  -- Make page body.
  (gridNodes, gridNodesStateElems) <- mkNodesGrid window acceptors
  panes <- UI.div #. show W3Row #+ panesAreas
  body
    <- UI.getBody window #+
         [ topNavigation allSelectors viewModeSelector metricsSelector
         , element panes
         ]

  UI.runFunction $ UI.ffi Chart.prepareChartsJS

  void $ UI.onEvent (UI.click paneViewButton) $ \_ -> do
    toggleViewMode window "paneMode" panes panesAreas [element gridNodes]
    forElementWithId window (show SelectMetricButton) hideIt
  void $ UI.onEvent (UI.click gridViewButton) $ \_ -> do
    toggleViewMode window "gridMode" panes [element gridNodes] panesAreas
    forElementWithId window (show SelectMetricButton) showIt
    forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
      UI.runFunction $ UI.ffi Chart.gridMemoryUsageChartJS  (show GridMemoryUsageChartId  <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridCPUUsageChartJS     (show GridCPUUsageChartId     <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridDiskUsageChartJS    (show GridDiskUsageChartId    <> nameOfNode)
      UI.runFunction $ UI.ffi Chart.gridNetworkUsageChartJS (show GridNetworkUsageChartId <> nameOfNode)

  forM_ acceptors $ \(RemoteAddrNamed nameOfNode _) -> do
    -- Charts for different metrics.
    UI.runFunction $ UI.ffi Chart.memoryUsageChartJS  (show MemoryUsageChartId  <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.cpuUsageChartJS     (show CPUUsageChartId     <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.diskUsageChartJS    (show DiskUsageChartId    <> nameOfNode)
    UI.runFunction $ UI.ffi Chart.networkUsageChartJS (show NetworkUsageChartId <> nameOfNode)

  nodesStateElems
    <- forM nodePanesWithElems $ \(nameOfNode, _, nodeStateElems, peerInfoItems) ->
         return (nameOfNode, nodeStateElems, peerInfoItems)

  return (body, (nodesStateElems, gridNodesStateElems))

topNavigation
  :: [UI Element]
  -> [UI Element]
  -> [UI Element]
  -> UI Element
topNavigation nodesSelector viewModeSelector metricsSelector =
  UI.div #. [W3Bar, W3Large] <+> [TopBar] #+
    [ UI.anchor #. show W3BarItem # set UI.href "https://iohk.io/" #+
        [ UI.img #. show IOHKLogo # set UI.src "/static/images/iohk-logo.png"
        ]
    , UI.div #. show W3DropdownHover #+
        [ UI.button ## show ViewModeButton
                    #. show W3Button
                    # set UI.value "paneMode"
                    #+ [string "View mode ▾"]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] <+> [] #+ viewModeSelector
        ]
    , UI.div #. show W3DropdownHover #+
        [ UI.button #. show W3Button #+ [string "Select node ▾"]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] <+> [] #+ nodesSelector
        ]
    , UI.div ## show SelectMetricButton #. show W3DropdownHover # hideIt #+
        [ UI.button #. show W3Button #+ [string "Select metric ▾"]
        , UI.div #. [W3DropdownContent, W3BarBlock, W3Card4] <+> [] #+ metricsSelector
        ]
    , UI.span #. [W3Right] <+> [ServiceName] #+
        [ string "Cardano Node Real-time View"
        ]
    ]

forNode
  :: Text
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> (UI Element -> UI Element)
  -> UI ()
forNode nameOfNode nodePanesWithElems action =
  forM_ nodePanesWithElems $ \(aName, pane, _, _) ->
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

showAllNodes
  :: UI.Window
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> UI ()
showAllNodes window nodePanesWithElems = do
  forM_ nodePanesWithElems $ \(_, pane, _, _) ->
    void $ element pane # showIt
  nodesCheckboxes <- UI.getElementsByClassName window (show SelectNodeCheck)
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked True

showAllNodesColumns
  :: UI.Window
  -> [(Text, Element, NodeStateElements, [PeerInfoItem])]
  -> UI ()
showAllNodesColumns window nodePanesWithElems = do
  forM_ nodePanesWithElems $ \(nameOfNode, _, _, _) ->
    forNodeColumn window nameOfNode showCell
  nodesCheckboxes <- UI.getElementsByClassName window (show SelectNodeCheck)
  forM_ nodesCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked True

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
  UI.getElementById window (show anId) >>= \case
    Just showAllButton -> do
      checkboxes <- UI.getElementsByClassName window (show aClass)
      statuses <- mapM (UI.get UI.checked) checkboxes
      if all ((==) True) statuses
        then void $ element showAllButton # set UI.class_ ([W3BarItem, W3Button, W3BorderTop, W3Disabled] <+> [])
        else void $ element showAllButton # set UI.class_ ([W3BarItem, W3Button, W3BorderTop] <+> [])
    Nothing -> return ()

mkMetricsSelector
  :: UI.Window
  -> UI [UI Element]
mkMetricsSelector window = do
  allMetricsButton <-
    UI.anchor ## show ShowAllMetricsButton
              #. [W3BarItem, W3Button, W3BorderTop, W3Disabled] <+> []
              # set UI.href "#"
              #+ [UI.string "Show all"]
  void $ UI.onEvent (UI.click allMetricsButton) $ \_ -> do
    showAllMetrics window allMetricsNames
    -- All metrics checkboxes are already shown, disable button again.
    void $ element allMetricsButton # set UI.class_ ([W3BarItem, W3Button, W3BorderTop, W3Disabled] <+> [])

  checkboxes <-
    forM allMetricsNames $ \aName ->
      element <$> mkCheckbox window aName
  return $ checkboxes ++ [element allMetricsButton]

mkCheckbox
  :: UI.Window
  -> ElementName
  -> UI Element
mkCheckbox window elemName = do
  metricCheckbox
    <- UI.input #. [W3Check] <+> [SelectMetricCheck]
                # set UI.type_ "checkbox"
                # set UI.checked True
                #+ []
  void $ UI.onEvent (UI.checkedChange metricCheckbox) $ \isChecked -> do
    let action = if isChecked then showRow else hideIt
    forElementWithId window (show elemName) action
    changeStatusOfShowAllButton window ShowAllMetricsButton SelectMetricCheck

  metricArea
    <- UI.div #. show SelectMetricCheckArea #+
         [ element metricCheckbox
         , UI.label #+ [UI.string $ metricLabel elemName]
         ]
  return metricArea

forElementWithId
  :: UI.Window
  -> String
  -> (UI Element -> UI Element)
  -> UI ()
forElementWithId window anId action =
  UI.getElementById window anId >>= \case
    Just el -> void $ element el # action
    Nothing -> return ()

showAllMetrics
  :: UI.Window
  -> [ElementName]
  -> UI ()
showAllMetrics window metricsElems = do
  forM_ metricsElems $ \elemName ->
    forElementWithId window (show elemName) showRow
  metricsCheckboxes <- UI.getElementsByClassName window (show SelectMetricCheck)
  forM_ metricsCheckboxes $ \checkbox ->
    void $ element checkbox # set UI.checked True
