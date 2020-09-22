{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.RTView.Config
    ( prepareConfigAndParams
    ) where

import           Cardano.Prelude

#if !defined(mingw32_HOST_OS)
import           Control.Monad (forM_)
#endif
import           Data.List (nub, nubBy)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.IO as TIO
import           Data.Yaml (ParseException, decodeFileEither, encodeFile)
import           System.Directory (XdgDirectory (..), doesFileExist,
                                   getTemporaryDirectory, getXdgDirectory)
#if !defined(mingw32_HOST_OS)
import           System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
#endif

import           System.FilePath ((</>))
#if defined(mingw32_HOST_OS)
import           System.FilePath (dropDrive)
#else
import           System.FilePath (takeDirectory)
#endif
import qualified System.Exit as Ex
import           Text.Read (readMaybe)

import           Cardano.BM.Configuration (Configuration, getAcceptAt, setup)
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.BackendKind (BackendKind (..))
import           Cardano.BM.Data.Configuration (RemoteAddr (..), RemoteAddrNamed (..))
import           Cardano.BM.Data.Output (ScribeDefinition (..), ScribeFormat (..),
                                         ScribeKind (..), ScribePrivacy (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Cardano.RTView.CLI (RTViewParams (..), defaultRTViewParams, defaultRTVPort,
                                     defaultRTVStatic)

-- | There are few possible ways how we can prepare RTView configuration:
--   1. By running interactive dialog with the user. If `--config` CLI-option
--      isn't provided, the user will answer few questions and actual configuration
--      will be prepared based on these answers.
--   2. By providing configuration explicitly. If `--config`, `--static` and `--port`
--      options are provided, these values will be used (interactive dialog will be skipped).
--   3. By using the last used configuration. If the user already launched
--      `cardano-rt-view-service` previously, the configuration was stored in
--      user's local directory (different for each supported platform),
--      and by default that configuration will be used again.
prepareConfigAndParams
  :: RTViewParams
  -> IO (Configuration, RTViewParams, [RemoteAddrNamed])
prepareConfigAndParams params' = do
  (config, params) <-
    if configFileIsProvided params'
      then do
        configFromFile <- readConfigFile $ rtvConfig params'
        return (configFromFile, params')
      else
        checkIfPreviousConfigExists >>= \case
          Just (prevConfig, prevParams) -> askUserAboutPrevConfig prevConfig prevParams
          Nothing -> startDialogToPrepareConfig
  acceptors <- checkIfTraceAcceptorIsDefined config
  makeSureTraceAcceptorsAreUnique acceptors
  -- To prevent TraceAcceptorPipeError "Network.Socket.bind: resource busy...
  rmPipesIfNeeded acceptors
  -- Configuration and parameters look good, save it for next sessions.
  saveConfigurationForNextSessions config
  saveRTViewParamsForNextSessions params
  return (config, params, acceptors)

configFileIsProvided :: RTViewParams -> Bool
configFileIsProvided params = not . null $ rtvConfig params

-- | Reads the service' configuration file (path is passed via '--config' CLI option).
readConfigFile :: FilePath -> IO Configuration
readConfigFile pathToConfig = setup pathToConfig `catch` exceptHandler
 where
  exceptHandler :: IOException -> IO Configuration
  exceptHandler e =
    Ex.die $ "Exception while reading configuration "
             <> pathToConfig
             <> ", exception: "
             <> show e

readRTViewParamsFile :: FilePath -> IO RTViewParams
readRTViewParamsFile pathToParams =
  decodeFileEither pathToParams >>= \case
    Left (e :: ParseException) -> Ex.die $ "Exception while reading RTView parameters "
                                         <> pathToParams <> ", exception: " <> show e
    Right (params :: RTViewParams) -> return params

-- | If `cardano-rt-view-service` already ws used on this computer,
--   the configuration was saved in user's local directory, which
--   differs on different platforms.
savedConfigurationFile :: IO FilePath
savedConfigurationFile = do
  -- For configuration files. It uses the XDG_CONFIG_HOME environment variable.
  -- On non-Windows systems, the default is ~/.config.
  -- On Windows, the default is %APPDATA% (e.g. C:/Users/<user>/AppData/Roaming).
  dir <- getXdgDirectory XdgConfig ""
  return $ dir </> "rt-view.yaml"

savedRTViewParamsFile :: IO FilePath
savedRTViewParamsFile = do
  dir <- getXdgDirectory XdgConfig ""
  return $ dir </> "rt-view-params.yaml"

checkIfPreviousConfigExists :: IO (Maybe (Configuration, RTViewParams))
checkIfPreviousConfigExists = do
  configExists <- savedConfigurationFile >>= doesFileExist
  paramsExist  <- savedRTViewParamsFile >>= doesFileExist
  if configExists && paramsExist
    then do
      config <- savedConfigurationFile >>= readConfigFile
      params <- savedRTViewParamsFile >>= readRTViewParamsFile
      return $ Just (config, params)
    else
      return Nothing

askUserAboutPrevConfig
  :: Configuration
  -> RTViewParams
  -> IO (Configuration, RTViewParams)
askUserAboutPrevConfig savedConfig savedParams = do
  TIO.putStrLn "Saved configuration file is found. Do you want to use it? <Y/N>"
  TIO.getLine >>= \case
    "Y" -> return (savedConfig, savedParams)
    "y" -> return (savedConfig, savedParams)
    ""  -> return (savedConfig, savedParams)
    "N" -> startDialogToPrepareConfig
    "n" -> startDialogToPrepareConfig
    _   -> startDialogToPrepareConfig

startDialogToPrepareConfig :: IO (Configuration, RTViewParams)
startDialogToPrepareConfig = do
  TIO.putStrLn $ "How many nodes will you connect (1 - "
               <> show maximumNode <> ", default is " <> show defaultNodesNumber <> "): "
  nodesNumber <- askAboutNodesNumber

  TIO.putStrLn $ "Input the names of the nodes (default are \""
               <> showDefaultNodesNames nodesNumber <> "\"): "
  nodesNames <- askAboutNodesNames nodesNumber

  TIO.putStrLn $ "Indicate the port for the web service (" <> show minimumPort
               <> " - " <> show maximumPort <> ", default is "
               <> show defaultRTVPort <> "): "
  port <- askAboutWebPort

  TIO.putStrLn "Connections shall be made via pipes (P, default way) or networking sockets (S)?"
  remoteAddrs <- askAboutPipesAndSockets >>= \case
    Pipe -> do
      defDir <- defaultPipesDir
      TIO.putStrLn $ "Ok, pipes will be used. Indicate the directory for them, default is \""
                     <> T.pack defDir <> "\": "
      askAboutLocationForPipes nodesNumber
    Socket -> do
      TIO.putStrLn $ "Ok, sockets will be used. Indicate the port base to listen for connections ("
                     <> show minimumPort <> " - " <> show maximumPort <> ", default is "
                     <> show defaultFirstPortForSockets <> "): "
      askAboutFirstPortForSockets nodesNumber

  TIO.putStrLn $ "Indicate the directory with static content for the web service, default is \""
                 <> T.pack defaultRTVStatic <> "\":"
  staticDir <- askAboutStaticDir

  -- Form configuration and params based on user's input.
  config <- CM.empty
  CM.setMinSeverity config Info
  CM.setSetupBackends config [KatipBK, LogBufferBK, TraceAcceptorBK]
  CM.setDefaultBackends config [KatipBK]
  CM.setSetupScribes config [ ScribeDefinition
                              { scName     = "stdout"
                              , scKind     = StdoutSK
                              , scFormat   = ScText
                              , scPrivacy  = ScPublic
                              , scMinSev   = Notice
                              , scMaxSev   = maxBound
                              , scRotation = Nothing
                              }
                            ]
  CM.setDefaultScribes config ["StdoutSK::stdout"]
  CM.setBackends config "cardano-rt-view.acceptor" (Just [ LogBufferBK
                                                         , UserDefinedBK "ErrorBufferBK"
                                                         ])
  let remoteAddrsNamed = map (\(nName, rAddr) -> RemoteAddrNamed nName rAddr)
                             $ zip nodesNames remoteAddrs
  CM.setAcceptAt config (Just remoteAddrsNamed)

  let params = defaultRTViewParams { rtvPort = port
                                   , rtvStatic = staticDir
                                   }
  return (config, params)

defaultNodesNumber :: Int
defaultNodesNumber = 3

maximumNode :: Int
maximumNode = 99

defaultNodeNamePrefix :: Text
defaultNodeNamePrefix = "node-"

defaultNodesNames :: Int -> [Text]
defaultNodesNames nodesNum = map (\nNum -> defaultNodeNamePrefix <> show nNum) [1 .. nodesNum]

showDefaultNodesNames :: Int -> Text
showDefaultNodesNames nodesNumber =
  T.intercalate "\", \"" $ defaultNodesNames nodesNumber

defaultFirstPortForSockets :: Int
defaultFirstPortForSockets = 3000

minimumPort, maximumPort :: Int
minimumPort = 1024
maximumPort = 65535

defaultPipesDir :: IO FilePath
defaultPipesDir = do
  tmp0 <- getTemporaryDirectory
  let tmp =
#if defined(mingw32_HOST_OS)
          "\\\\.\\pipe\\" ++ (T.unpack . T.replace "\\" "-" . T.pack) (dropDrive tmp0) ++ "_"
#else
          tmp0 ++ "/"
#endif
  return $ tmp ++ "rt-view-pipes"

askAboutNodesNumber :: IO Int
askAboutNodesNumber = do
  nodesNumberRaw <- TIO.getLine
  nodesNumber <-
    if T.null nodesNumberRaw
      then return defaultNodesNumber
      else case readMaybe (T.unpack nodesNumberRaw) of
             Just (n :: Int) -> return n
             Nothing -> do
               TIO.putStrLn "It's not a number, please input the number instead: "
               askAboutNodesNumber
  if nodesNumber < 1 || nodesNumber > maximumNode
    then do
      TIO.putStrLn $ "Wrong number of nodes, please input the number from 1 to "
                     <> show maximumNode <> ": "
      askAboutNodesNumber
    else do
      TIO.putStrLn $ "Ok, " <> show nodesNumber <> " nodes."
      return nodesNumber

askAboutNodesNames :: Int -> IO [Text]
askAboutNodesNames nodesNumber = askNodeNames 1
 where
   askNodeNames :: Int -> IO [Text]
   askNodeNames i = do
     aName <- askNodeName i
     if | T.null aName && i == 1 -> do
            TIO.putStrLn $ "Ok, default names \"" <> showDefaultNodesNames nodesNumber
                           <> "\" will be used."
            return $ defaultNodesNames nodesNumber
        | i == nodesNumber -> do
            TIO.putStrLn $ "Ok, the last node has name \"" <> aName <> "\""
            return $ aName : []
        | otherwise -> do
            TIO.putStrLn $ "Ok, node " <> show i <> " has name \"" <> aName
                           <> "\", input the next one:"
            names <- askNodeNames (i + 1)
            return $ aName : names

   askNodeName :: Int -> IO Text
   askNodeName i = do
    aName <- TIO.getLine
    if | T.null aName && i == 1 ->
           return ""
       | T.null aName && i /= 1 -> do
           TIO.putStrLn "Node's name cannot be empty, please input again: "
           askNodeName i
       | T.any (== ' ') aName -> do
           TIO.putStrLn "Node's name cannot contain spaces, please input again:  "
           askNodeName i
       | otherwise -> return aName

askAboutWebPort :: IO Int
askAboutWebPort = do
  portRaw <- TIO.getLine
  port <-
    if T.null portRaw
      then return defaultRTVPort
      else case readMaybe (T.unpack portRaw) of
             Just (n :: Int) -> return n
             Nothing -> do
               TIO.putStrLn "It's not a number, please input the number instead: "
               askAboutWebPort
  if port < minimumPort || port > maximumPort
    then do
      TIO.putStrLn $ "Please choose the port between " <> show minimumPort <> " and "
                   <> show maximumPort <> ": "
      askAboutWebPort
    else do
      TIO.putStrLn $ "Ok, the service will be listening on http://127.0.0.1:" <> show port
      return port

data ConnectionWay
  = Pipe
  | Socket

askAboutPipesAndSockets :: IO ConnectionWay
askAboutPipesAndSockets = do
  decisionRaw <- TIO.getLine
  case decisionRaw of
    "P" -> return Pipe
    "p" -> return Pipe
    ""  -> return Pipe
    "S" -> return Socket
    "s" -> return Socket
    _   -> return Pipe

askAboutLocationForPipes :: Int -> IO [RemoteAddr]
askAboutLocationForPipes nodesNumber = do
  dir <- T.strip <$> TIO.getLine
  if T.null dir
    then do
      defDir <- defaultPipesDir
      TIO.putStrLn $ "Ok, default directory \"" <> T.pack defDir <> "\" will be used."
      mkdir defDir
      return $ mkpipe defDir
    else do
      TIO.putStrLn $ "Ok, directory \"" <> dir <> "\" will be used for the pipes."
      mkdir (T.unpack dir)
      return $ mkpipe (T.unpack dir)
 where
  mkdir :: FilePath -> IO ()
  prepname :: FilePath -> Text -> FilePath
#if defined(mingw32_HOST_OS)
  mkdir _ = pure ()
  prepname d n = d <> "_" <> T.unpack n
#else
  mkdir = createDirectoryIfMissing True
  prepname d n = d </> T.unpack n
#endif
  mkpipe :: FilePath -> [RemoteAddr]
  mkpipe d = map (\defName -> RemotePipe (prepname d defName)) $
                 defaultNodesNames nodesNumber

askAboutFirstPortForSockets :: Int -> IO [RemoteAddr]
askAboutFirstPortForSockets nodesNumber = do
  firstPort <- askAboutFirstPort
  let portsForAllNodes = [firstPort .. firstPort + nodesNumber - 1]
  TIO.putStrLn $ "Ok, these ports will be used to accept nodes' metrics: " <> showPorts portsForAllNodes
  return $ map (\p -> RemoteSocket "127.0.0.1" (show p)) portsForAllNodes
 where
  showPorts :: [Int] -> Text
  showPorts ports = T.intercalate ", " $ map (T.pack . show) ports

askAboutFirstPort :: IO Int
askAboutFirstPort = do
  portRaw <- TIO.getLine
  port <-
    if T.null portRaw
      then return defaultFirstPortForSockets
      else case readMaybe (T.unpack portRaw) of
             Just (n :: Int) -> return n
             Nothing -> do
               TIO.putStrLn "It's not a number, please input the number instead: "
               askAboutFirstPort
  if port < minimumPort || port > maximumPort
    then do
      TIO.putStrLn $ "Please choose the port between " <> show minimumPort <> " and "
                   <> show maximumPort <> ": "
      askAboutFirstPort
    else
      return port

askAboutStaticDir :: IO FilePath
askAboutStaticDir = do
  dir <- T.strip <$> TIO.getLine
  if T.null dir
    then do
      TIO.putStrLn $ "Ok, default directory will be used."
      return defaultRTVStatic
    else do
      TIO.putStrLn $ "Ok, static content will be taken from directory \"" <> dir <> "\"."
      return $ T.unpack dir

rmPipesIfNeeded :: [RemoteAddrNamed] -> IO ()
#if defined(mingw32_HOST_OS)
rmPipesIfNeeded _ = pure ()
#else
rmPipesIfNeeded acceptors = do
  let pipesDirs = map collectPipesDirs acceptors
  forM_ pipesDirs $ \dir ->
    when (not . null $ dir) $ do
      allFiles <- listDirectory dir
      let allPipes = filter (\file -> defaultNodeNamePrefix `T.isPrefixOf` (T.pack file)) allFiles
      forM_ allPipes $ \pipe -> removeFile (dir </> pipe)
 where
  collectPipesDirs (RemoteAddrNamed _ (RemoteSocket _ _)) = ""
  collectPipesDirs (RemoteAddrNamed _ (RemotePipe path)) = takeDirectory path
#endif

saveConfigurationForNextSessions :: Configuration -> IO ()
saveConfigurationForNextSessions config = do
  path <- savedConfigurationFile
  CM.toRepresentation config >>= encodeFile path

saveRTViewParamsForNextSessions :: RTViewParams -> IO ()
saveRTViewParamsForNextSessions params = do
  path <- savedRTViewParamsFile
  encodeFile path params

-- | RTView service requires at least one |TraceAcceptor|.
checkIfTraceAcceptorIsDefined
  :: Configuration
  -> IO [RemoteAddrNamed]
checkIfTraceAcceptorIsDefined config =
  getAcceptAt config >>= \case
    Just acceptors -> return acceptors
    Nothing -> Ex.die "No trace acceptors found in the configuration, please add at leas one."

-- | If configuration contains more than one trace acceptor,
--   check if they are unique, to avoid socket problems.
makeSureTraceAcceptorsAreUnique
  :: [RemoteAddrNamed]
  -> IO ()
makeSureTraceAcceptorsAreUnique acceptors = do
  checkIfNodesNamesAreUnique
  checkIfNetParametersAreUnique
 where
  checkIfNodesNamesAreUnique =
    when (length names /= length (nub names)) $
      Ex.die "Nodes' names in trace acceptors must be unique!"

  checkIfNetParametersAreUnique =
    when (length addrs /= length (nubBy compareNetParams addrs)) $
      Ex.die "Nodes' network parameters in trace acceptors must be unique!"

  compareNetParams (RemoteSocket h1 p1) (RemoteSocket h2 p2) = h1 == h2 && p1 == p2
  compareNetParams (RemoteSocket _ _)   (RemotePipe _)       = False
  compareNetParams (RemotePipe _)       (RemoteSocket _ _)   = False
  compareNetParams (RemotePipe p1)      (RemotePipe p2)      = p1 == p2

  names = [name | RemoteAddrNamed name _ <- acceptors]
  addrs = [addr | RemoteAddrNamed _ addr <- acceptors]
