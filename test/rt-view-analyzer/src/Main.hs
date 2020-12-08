module Main
  (
    main
  )
where

import           Test.WebDriver

import           Analyzers (Units (..), waitFor)
import           CLI (parseArguments)
import           Config (getAcceptorInfoFrom, readRTViewConfig, wdConfig)

main :: IO ()
main = do
  (pathToRTViewConfig, _logObjectsJSON, rtViewWebPort) <- parseArguments
  rtViewConfig <- readRTViewConfig pathToRTViewConfig
  (_nodeName, _unixSocket) <- getAcceptorInfoFrom rtViewConfig

  runSession wdConfig . closeOnException $ do
    -- We always test cardano-rt-view launched locally, so we only need a web port.
    let rtViewPageURL = "http://127.0.0.1:" <> show rtViewWebPort
    openPage rtViewPageURL
    waitFor 2 Seconds

    -- TODO

    closeSession
