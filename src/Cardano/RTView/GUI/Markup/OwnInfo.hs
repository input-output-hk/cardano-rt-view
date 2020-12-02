{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.GUI.Markup.OwnInfo
    ( mkOwnInfo
    ) where

import           Control.Monad (forM, void)
import           Data.List (intersperse)
import qualified Data.Text as T
import           Data.Version (showVersion)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, (#), (#+))

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.Config (configFileIsProvided, savedConfigurationFile,
                                        savedRTViewParamsFile)
import           Cardano.RTView.GUI.Elements (HTMLClass (..), (#.), hideIt)
import           Cardano.RTView.GUI.JS.Utils (copyTextToClipboard)
import           Cardano.RTView.Git.Rev (gitRev)
import           Cardano.RTView.SupportedNodes (supportedNodesVersions)
import           Paths_cardano_rt_view (version)

mkOwnInfo :: RTViewParams -> UI Element
mkOwnInfo params = do
  closeButton <- UI.img #. [W3DisplayTopright, RTViewInfoClose]
                        # set UI.src "/static/images/times.svg"
                        # set UI.title__ "Close"
  versions <- nodesVersions
  (pathToConfigFile, pathToParamsFile) <- liftIO $ getPathsToConfigAndParamsFiles params

  copyPathToConfigFile
    <- UI.img #. [RTViewInfoCopyPathIcon]
              # set UI.src "/static/images/clipboard.svg"
              # set UI.title__ "Copy this path to clipboard"
  copyPathToParamsFile
    <- UI.img #. [RTViewInfoCopyPathIcon]
              # set UI.src "/static/images/clipboard.svg"
              # set UI.title__ "Copy this path to clipboard"
  void $ UI.onEvent (UI.click copyPathToConfigFile) $ \_ -> do
    UI.runFunction $ UI.ffi copyTextToClipboard pathToConfigFile
  void $ UI.onEvent (UI.click copyPathToParamsFile) $ \_ -> do
    UI.runFunction $ UI.ffi copyTextToClipboard pathToParamsFile

  let rtViewVersion = showVersion version

  ownInfo <-
    UI.div #. [W3Modal] #+
      [ UI.div #. [W3ModalContent, W3AnimateTop, W3Card4] #+
          [ UI.div #. [W3Container, RTViewInfoTop] #+
              [ element closeButton
              , UI.h2 #+ [ string "RTView Info" ]
              ]
          , UI.div #. [W3Container, W3Row, RTViewInfoContainer] #+
              [ UI.div #. [W3Half] #+
                  [ vSpacer
                  , UI.div #+ [string "Version"  # set UI.title__ "Version of RTView"]
                  , UI.div #+ [string "Commit"   # set UI.title__ "Git commit RTView was built from"]
                  , UI.div #+ [string "Platform" # set UI.title__ "Platform RTView is working on"]
                  , vSpacer
                  , UI.div #+ [string "Supported nodes" # set UI.title__ "Versions of the nodes RTView was tested with"]
                  , vSpacer
                  , UI.div #+ [string "Configuration file" # set UI.title__ "The path to RTView configuration file"]
                  , UI.div #+ [string "Parameters file"    # set UI.title__ "The path to RTView parameters file"]
                  , vSpacer
                  ]
              , UI.div #. [W3Half, NodeInfoValues] #+
                  [ vSpacer
                  , UI.div #+
                      [ UI.anchor #. [CommitLink]
                                  # set UI.href ("https://github.com/input-output-hk/cardano-rt-view/releases/tag/"
                                                 <> rtViewVersion)
                                  # set UI.target "_blank"
                                  # set UI.title__ ("See release tag "
                                                    <> rtViewVersion <> " in cardano-rt-view repository")
                                  # set UI.text rtViewVersion
                      ]
                  , UI.div #+
                      [ UI.anchor #. [CommitLink]
                                  # set UI.href ("https://github.com/input-output-hk/cardano-rt-view/commit/"
                                                 <> gitRev)
                                  # set UI.target "_blank"
                                  # set UI.title__ "Browse cardano-rt-view repository on this commit"
                                  # set UI.text (take 7 gitRev)
                      ]
                  , UI.div #+ [string rtViewPlaform]
                  , vSpacer
                  , UI.div #+ (intersperse (string ", ") versions)
                  , vSpacer
                  , UI.div #+
                      [ string (preparePathIfNeeded pathToConfigFile) # set UI.title__ pathToConfigFile
                      , element copyPathToConfigFile
                      ]
                  , UI.div #+
                      [ string (preparePathIfNeeded pathToParamsFile) # set UI.title__ pathToParamsFile
                      , element copyPathToParamsFile
                      ]
                  , vSpacer
                  ]
             ]
          ]
      ]
  void $ UI.onEvent (UI.click closeButton) $ \_ -> do
    element ownInfo # hideIt

  return ownInfo

vSpacer :: UI Element
vSpacer = UI.div #. [NodeInfoVSpacer] #+ []

rtViewPlaform :: String
#if defined(mingw32_HOST_OS)
rtViewPlaform = "Windows"
#elif defined(linux_HOST_OS)
rtViewPlaform = "Linux"
#elif defined(darwin_HOST_OS)
rtViewPlaform = "macOS"
#else
rtViewPlaform = "Unknown"
#endif

nodesVersions :: UI [UI Element]
nodesVersions =
  forM supportedNodesVersions $ \ver -> do
    element <$> UI.anchor #. [CommitLink]
                          # set UI.href ("https://github.com/input-output-hk/cardano-node/releases/tag/"
                               <> T.unpack ver)
                          # set UI.target "_blank"
                          # set UI.title__ ("See release tag " <> T.unpack ver <> " in cardano-node repository")
                          # set UI.text (T.unpack ver)

getPathsToConfigAndParamsFiles :: RTViewParams -> IO (FilePath, FilePath)
getPathsToConfigAndParamsFiles params = do
  configFile <-
    if configFileIsProvided params
      then return $ rtvConfig params
      else savedConfigurationFile
  paramsFile <- savedRTViewParamsFile
  return (configFile, paramsFile)

preparePathIfNeeded :: FilePath -> String
preparePathIfNeeded aPath = if tooLongPath then shortenedPath else aPath
 where
  tooLongPath = len > 20
  len = length aPath
  shortenedPath = take 10 aPath <> "..." <> drop (len - 10) aPath
