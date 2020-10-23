module Main
  (
    main
  )
where

import           Control.Monad (forM_)

import           Data.Text (pack)
import           Test.WebDriver

import           Cardano.RTView.GUI.Elements (HTMLId (..))

import           Analyzers (Units (..), ViewMode (..), changeViewModeTo, checkContentOf,
                            checkIfMetricCanBeHiddenOrShown, checkIfNodeCanBeHiddenOrShown,
                            metricsRowsWithChecks, waitFor)
import           CLI (parseArguments)
import           Config (getAcceptorInfoFrom, readRTViewConfig, wdConfig)

main :: IO ()
main = do
  (pathToRTViewConfig, _logObjectsJSON, rtViewWebPort) <- parseArguments
  rtViewConfig <- readRTViewConfig pathToRTViewConfig
  (nodeName, unixSocket) <- getAcceptorInfoFrom rtViewConfig

  runSession wdConfig . closeOnException $ do
    -- We always test cardano-rt-view launched locally, so we only need a web port.
    let rtViewPageURL = "http://127.0.0.1:" <> show rtViewWebPort
    openPage rtViewPageURL
    waitFor 2 Seconds

    --- Test Grid mode ---
    changeViewModeTo Grid

    checkContentOf ("ElTraceAcceptorEndpoint-" <> nodeName) unixSocket "TraceAcceptor endpoint"
    waitFor 1000 Milliseconds

    findElem (ById (pack . show $ SelectMetricButton)) >>= click
    waitFor 2 Seconds

    forM_ metricsRowsWithChecks $ \(rowId, checkId) ->
      checkIfMetricCanBeHiddenOrShown rowId checkId

    checkIfNodeCanBeHiddenOrShown nodeName

    --- Test Pane mode ---
    changeViewModeTo Pane
    waitFor 2 Seconds
    -- TODO

    closeSession
