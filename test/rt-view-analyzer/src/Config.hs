{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
  (
    getAcceptorInfoFrom
  , readRTViewConfig
  , wdConfig
  )
where

import           Control.Exception (IOException, catch, throwIO)
import           Control.Monad (when)
import           Data.Text (Text, pack)
import           Test.WebDriver

import           Cardano.BM.Configuration (Configuration, getAcceptAt, setup)
import           Cardano.BM.Data.Configuration (RemoteAddr (..), RemoteAddrNamed (..))

-- Please note that using of Firefox browser assumes that GeckoDriver is installed as well.
wdConfig :: WDConfig
wdConfig = defaultConfig { wdCapabilities = caps }
 where
  caps = defaultCaps { browser = firefox }

readRTViewConfig :: FilePath -> IO Configuration
readRTViewConfig fp = setup fp `catch` problems
 where
  problems :: IOException -> IO a
  problems e = do
    putStrLn $ "Exception while reading RTView configuration from: " <> fp
    throwIO e

getAcceptorInfoFrom :: Configuration -> IO (Text, Text)
getAcceptorInfoFrom rtViewConfig =
  getAcceptAt rtViewConfig >>= \case
    Just acceptors -> do
      when (null acceptors) $ error "Trace acceptor doesn't contain RemoteAddrNamed."
      -- We only need one acceptor to test RTView UI.
      let (fstAcceptor : _) = acceptors
      case remoteAddr fstAcceptor of
        RemotePipe unixSocket -> return (nodeName fstAcceptor, pack unixSocket)
        RemoteSocket _ _ -> error "Trace acceptor uses addr:port instead of UNIX socket."
    Nothing -> error "Trace acceptor not enabled in RTView config."
