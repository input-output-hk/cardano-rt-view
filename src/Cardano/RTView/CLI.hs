{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.RTView.CLI
    ( RTViewParams (..)
    , defaultRTViewParams
    , defaultRTVConfig
    , defaultRTVStatic
    , defaultRTVPort
    , defaultRTVNodeInfoLife
    , defaultRTVBlockchainInfoLife
    , defaultRTVResourcesInfoLife
    , defaultRTVRTSInfoLife
    , parseRTViewParams
    ) where

import           Data.Word (Word64)
import           Data.Yaml (FromJSON (..), ToJSON, (.:), withObject)
import           GHC.Generics (Generic)

import           Options.Applicative (HasCompleter, HasMetavar, HasName, HasValue, Mod, Parser,
                                      auto, bashCompleter, completer, help, long, metavar, option,
                                      showDefault, strOption, value)

-- | Type for CLI parameters required for the service.
data RTViewParams
  = RTViewParams
      { rtvConfig             :: !FilePath
      , rtvStatic             :: !FilePath
      , rtvPort               :: !Int
      , rtvNodeInfoLife       :: !Word64
      , rtvBlockchainInfoLife :: !Word64
      , rtvResourcesInfoLife  :: !Word64
      , rtvRTSInfoLife        :: !Word64
      } deriving (Generic, ToJSON)

instance FromJSON RTViewParams where
  parseJSON = withObject "RTViewParams" $ \v -> RTViewParams
    <$> v .:  "rtvConfig"
    <*> v .:  "rtvStatic"
    <*> v .:  "rtvPort"
    <*> v .:  "rtvNodeInfoLife"
    <*> v .:  "rtvBlockchainInfoLife"
    <*> v .:  "rtvResourcesInfoLife"
    <*> v .:  "rtvRTSInfoLife"

defaultRTViewParams :: RTViewParams
defaultRTViewParams = RTViewParams
  { rtvConfig             = defaultRTVConfig
  , rtvStatic             = defaultRTVStatic
  , rtvPort               = defaultRTVPort
  , rtvNodeInfoLife       = defaultRTVNodeInfoLife
  , rtvBlockchainInfoLife = defaultRTVBlockchainInfoLife
  , rtvResourcesInfoLife  = defaultRTVResourcesInfoLife
  , rtvRTSInfoLife        = defaultRTVRTSInfoLife
  }

defaultRTVConfig, defaultRTVStatic :: FilePath
defaultRTVConfig = ""
defaultRTVStatic = "static"

defaultRTVPort :: Int
defaultRTVPort = 8024

defaultRTVNodeInfoLife
  , defaultRTVBlockchainInfoLife
  , defaultRTVResourcesInfoLife
  , defaultRTVRTSInfoLife :: Word64
defaultRTVNodeInfoLife       = secToNanosec 5
defaultRTVBlockchainInfoLife = secToNanosec 35
defaultRTVResourcesInfoLife  = secToNanosec 35
defaultRTVRTSInfoLife        = secToNanosec 45

secToNanosec :: Int -> Word64
secToNanosec s = fromIntegral $ s * 1000000000

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
    <*> parseDiffTime
          "node-info-life"
          "Lifetime of node info"
          defaultRTVNodeInfoLife
    <*> parseDiffTime
          "blockchain-info-life"
          "Lifetime of blockchain info"
          defaultRTVBlockchainInfoLife
    <*> parseDiffTime
          "resources-info-life"
          "Lifetime of resources info"
          defaultRTVResourcesInfoLife
    <*> parseDiffTime
          "rts-info-life"
          "Lifetime of GHC RTS info"
          defaultRTVRTSInfoLife

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

parseDiffTime
  :: String
  -> String
  -> Word64
  -> Parser Word64
parseDiffTime optname desc defaultTime =
  option (secToNanosec <$> auto) (
       long optname
    <> metavar "DIFFTIME"
    <> help desc
    <> value defaultTime
    <> showDefault
  )
