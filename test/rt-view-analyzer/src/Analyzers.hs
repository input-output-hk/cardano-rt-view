module Analyzers
  (
    Units (..)
  , ViewMode (..)
  , changeViewModeTo
  , checkContentOf
  , checkIfMetricCanBeHiddenOrShown
  , checkIfNodeCanBeHiddenOrShown
  , metricsRowsWithChecks
  , waitFor
  )
where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text, pack, unpack)
import           System.Exit (die)

import           Test.WebDriver

import           Cardano.RTView.GUI.Elements (HTMLId (..))
import           Cardano.RTView.GUI.Grid (allMetricsNames)

data Units = Seconds | Milliseconds

waitFor
  :: Int
  -> Units
  -> WD ()
waitFor howMany Seconds      = liftIO . threadDelay $ howMany * 1000000
waitFor howMany Milliseconds = liftIO . threadDelay $ howMany * 1000

data ViewMode = Pane | Grid

instance Show ViewMode where
  show Pane = "Pane view"
  show Grid = "Grid view"

changeViewModeTo :: ViewMode -> WD ()
changeViewModeTo mode = do
  findElem (ById (pack . show $ ViewModeButton)) >>= click
  waitFor 2 Seconds
  findElem (ByLinkText (pack . show $ mode)) >>= click
  waitFor 3 Seconds

checkContentOf
  :: Text
  -> Text
  -> Text
  -> WD ()
checkContentOf elementId content label = do
  realContent <- findElem (ById elementId) >>= getText
  when (realContent /= content) $
    liftIO . die $ unpack label <> " is wrong: expected " <> unpack content
                                <> ", but got " <> unpack realContent

metricsRowsWithChecks :: [(Text, Text)]
metricsRowsWithChecks = zip metricsRowsIds metricsChecksIds
 where
  metricsRowsIds = map (pack . show) allMetricsNames
  metricsChecksIds =
    map (\n -> "#" <> (pack . show $ SelectMetricButton)
                   <> " > div > div:nth-child(" <> (pack . show $ n) <> ") > input")
        [1 .. length allMetricsNames]

checkIfMetricCanBeHiddenOrShown
  :: Text
  -> Text
  -> WD ()
checkIfMetricCanBeHiddenOrShown rowId checkId = do
  metricCheck <- findElem (ByCSS checkId)
  metricRow <- findElem (ById rowId)

  click metricCheck
  waitFor 100 Milliseconds

  isDisplayed metricRow >>= \visible -> when visible $
    liftIO . die $ "Metric's row " <> unpack rowId <> " should be hidden now, but it's visible."

  click metricCheck
  waitFor 100 Milliseconds

  isDisplayed metricRow >>= \visible -> when (not visible) $
    liftIO . die $ "Metric's row " <> unpack rowId <> " should be visible now, but it's hidden."

checkIfNodeCanBeHiddenOrShown :: Text -> WD ()
checkIfNodeCanBeHiddenOrShown nodeName = do
  findElem (ByCSS "body > div.w3-bar.w3-large.TopBar > div:nth-child(3) > button") >>= click
  waitFor 2 Seconds

  nodeCheck <- findElem (ByCSS "body > div.w3-bar.w3-large.TopBar > div:nth-child(3) > div > div > input")
  waitFor 2 Seconds

  click nodeCheck
  waitFor 200 Milliseconds

  forM_ metricsForOneNode $ \metricCellId ->
    findElem (ById metricCellId) >>= isDisplayed >>= \visible -> when visible $
      liftIO . die $ "Metric's cell " <> unpack metricCellId <> " should be hidden now, but it's visible."

  click nodeCheck
  waitFor 200 Milliseconds

  forM_ metricsForOneNode $ \metricCellId ->
    findElem (ById metricCellId) >>= isDisplayed >>= \visible -> when (not visible) $
      liftIO . die $ "Metric's cell " <> unpack metricCellId <> " should be visible now, but it's hidden."
 where
  metricsForOneNode = map (\mName -> (pack . show $ mName) <> "-" <> nodeName) allMetricsNames
