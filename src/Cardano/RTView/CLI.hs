{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.RTView.CLI
    ( RTViewParams (..)
    , defaultRTViewParams
    , defaultRTVConfig
    , defaultRTVStatic
    , defaultRTVPort
    , defaultRTVActiveNodeLife
    , parseRTViewParams
    ) where

import           Data.Aeson (FromJSON (..), ToJSON, (.:), withObject)
import           GHC.Generics (Generic)
import           Options.Applicative (HasCompleter, HasMetavar, HasName, HasValue, Mod, Parser,
                                      auto, bashCompleter, completer, help, long, metavar, option,
                                      showDefault, strOption, value)

-- | Type for CLI parameters required for the service.
data RTViewParams = RTViewParams
  { rtvConfig         :: !FilePath
  , rtvStatic         :: !FilePath
  , rtvPort           :: !Int
  , rtvActiveNodeLife :: !Int
  } deriving (Generic, ToJSON)

instance FromJSON RTViewParams where
  parseJSON = withObject "RTViewParams" $ \v -> RTViewParams
    <$> v .: "rtvConfig"
    <*> v .: "rtvStatic"
    <*> v .: "rtvPort"
    <*> v .: "rtvActiveNodeLife"

defaultRTViewParams :: RTViewParams
defaultRTViewParams = RTViewParams
  { rtvConfig         = defaultRTVConfig
  , rtvStatic         = defaultRTVStatic
  , rtvPort           = defaultRTVPort
  , rtvActiveNodeLife = defaultRTVActiveNodeLife
  }

defaultRTVConfig, defaultRTVStatic :: FilePath
defaultRTVConfig = ""
defaultRTVStatic = "static"

defaultRTVPort :: Int
defaultRTVPort = 8024

defaultRTVActiveNodeLife :: Int
defaultRTVActiveNodeLife = 120

parseRTViewParams :: Parser RTViewParams
parseRTViewParams =
  RTViewParams
    <$> parseFilePath
          "config"
          "file"
          "Configuration file for RTView service. If not provided, interactive dialog will be started."
          defaultRTVConfig
    <*> parseFilePath
          "static"
          "directory"
          "Directory with static content"
          defaultRTVStatic
    <*> parseInt
          "port"
          "The port number"
          "PORT"
          defaultRTVPort
    <*> parseInt
          "active-node-life"
          "Active node lifetime, in seconds"
          "TIME"
          defaultRTVActiveNodeLife

-- Aux parsers

parseFilePath
  :: String
  -> String
  -> String
  -> FilePath
  -> Parser FilePath
parseFilePath optname completion desc defaultPath =
  let flags :: (HasCompleter f, HasMetavar f, HasName f, HasValue f)
            => Mod f FilePath
      flags = long optname
           <> metavar "FILEPATH"
           <> help desc
           <> completer (bashCompleter completion)
           <> value defaultPath
  in strOption $ if null defaultPath
                   then flags
                   else flags <> showDefault

parseInt
  :: String
  -> String
  -> String
  -> Int
  -> Parser Int
parseInt optname desc metavar' defaultValue =
  option auto (
       long optname
    <> metavar metavar'
    <> help desc
    <> value defaultValue
    <> showDefault
  )
