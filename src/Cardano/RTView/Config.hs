{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Cardano.RTView.Config
    ( prepareConfigAndParams
    , configFileIsProvided
    , notificationsFileIsProvided
    , savedConfigurationFile
    , savedNotificationsFile
    , savedRTViewParamsFile
    , saveNotificationsForNextSessions
    , logFilesDir
    ) where

import           Control.Exception (IOException, catch)
import           Control.Monad (forM_, unless, void, when)
import           Data.Aeson (eitherDecodeFileStrict)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (nub, nubBy)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.IO as TIO
import           System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleIntensity (..),
                                      ConsoleLayer (..), SGR (..), setSGR)
import           System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist,
                                   getTemporaryDirectory, getXdgDirectory)
#if !defined(mingw32_HOST_OS)
import           System.Directory (listDirectory, removeFile)
#endif
#if defined(mingw32_HOST_OS)
import           System.FilePath ((</>), dropDrive)
#else
import           System.FilePath ((</>), takeDirectory)
#endif
import qualified System.Exit as Ex
import           System.IO (hFlush, stdout)
import           Text.Read (readMaybe)

import           Cardano.BM.Configuration (Configuration, getAcceptAt, setup)
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.BackendKind (BackendKind (..))
import           Cardano.BM.Data.Configuration (RemoteAddr (..), RemoteAddrNamed (..))
import           Cardano.BM.Data.Output (ScribeDefinition (..), ScribeFormat (..),
                                         ScribeKind (..), ScribePrivacy (..))
import           Cardano.BM.Data.Rotation (RotationParameters (..))
import           Cardano.BM.Data.Severity (Severity (..))

import           Cardano.RTView.CLI (RTViewParams (..), defaultRTViewParams, defaultRTVPort,
                                     defaultRTVStatic)
import           Cardano.RTView.SupportedNodes (showSupportedNodesVersions)
import           Cardano.RTView.Notifications.Types

-- | There are few possible ways how we can prepare RTView configuration:
--   1. By running interactive dialog with the user. If `--config` CLI-option
--      isn't provided, the user will answer few questions and actual configuration
--      will be prepared based on these answers.
--   2. By providing configuration explicitly. If `--config`, `--static` and `--port`
--      options are provided, these values will be used (interactive dialog will be skipped).
--   3. By using the last used configuration. If the user already launched
--      `cardano-rt-view` previously, the configuration was stored in
--      user's local directory (different for each supported platform),
--      and by default that configuration will be used again.
prepareConfigAndParams
  :: RTViewParams
  -> IO (Configuration, NotificationSettings, RTViewParams, [RemoteAddrNamed])
prepareConfigAndParams params' = do
  (config, params) <-
    if configFileIsProvided params'
      then do
        configFromFile <- readConfigFile $ rtvConfig params'
        return (configFromFile, params')
      else
        checkIfPreviousConfigExists >>= \case
          Just (prevConfig, prevParams) -> do
            colorize Magenta NormalIntensity $
              TIO.putStr "\nSaved configuration file is found. Do you want to use it? <Y/N>: "
            askAboutPrevConfig prevConfig prevParams
          Nothing ->
            startDialogToPrepareConfig
  notifySettings <-
    if notificationsFileIsProvided params'
      then readNotificationsFile $ rtvNotifications params'
      else checkIfPreviousNotificationsExists >>= \case
        Just prevNotify -> return prevNotify
        Nothing -> return initialNotificationSettings

  acceptors <- checkIfTraceAcceptorIsDefined config
  makeSureTraceAcceptorsAreUnique acceptors
  -- To prevent TraceAcceptorPipeError "Network.Socket.bind: resource busy...
  rmPipesIfNeeded acceptors
  -- Configuration and parameters look good, save it for next sessions.
  saveConfigurationForNextSessions config
  saveNotificationsForNextSessions notifySettings
  saveRTViewParamsForNextSessions params
  return (config, notifySettings, params, acceptors)

configFileIsProvided, notificationsFileIsProvided :: RTViewParams -> Bool
configFileIsProvided params = not . null $ rtvConfig params
notificationsFileIsProvided params = not . null $ rtvNotifications params

-- | Reads the program's configuration file (path is passed via '--config' CLI option).
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
  eitherDecodeFileStrict pathToParams >>= \case
    Left errMsg -> Ex.die $ "Error while reading RTView parameters "
                            <> pathToParams <> ", error: " <> errMsg
    Right (params :: RTViewParams) -> return params

readNotificationsFile :: FilePath -> IO NotificationSettings
readNotificationsFile pathToNotify =
  eitherDecodeFileStrict pathToNotify >>= \case
    Left errMsg -> Ex.die $ "Error while reading notifications file "
                            <> pathToNotify <> ", error: " <> errMsg
    Right (notify :: NotificationSettings) -> return notify

-- | If `cardano-rt-view` already was used on this computer,
--   the configuration was saved in user's local directory, which
--   differs on different platforms.
savedConfigurationFile :: IO FilePath
savedConfigurationFile = do
  -- For configuration files. It uses the XDG_CONFIG_HOME environment variable.
  -- On non-Windows systems, the default is ~/.config.
  -- On Windows, the default is %APPDATA% (e.g. C:/Users/<user>/AppData/Roaming).
  dir <- getXdgDirectory XdgConfig ""
  createDirectoryIfMissing True dir
  return $ dir </> "cardano-rt-view.json"

savedNotificationsFile :: IO FilePath
savedNotificationsFile = do
  dir <- getXdgDirectory XdgConfig ""
  createDirectoryIfMissing True dir
  return $ dir </> "cardano-rt-view-notifications.json"

savedRTViewParamsFile :: IO FilePath
savedRTViewParamsFile = do
  dir <- getXdgDirectory XdgConfig ""
  createDirectoryIfMissing True dir
  return $ dir </> "cardano-rt-view-params.json"

logFilesDir :: IO FilePath
logFilesDir = do
  dir <- getXdgDirectory XdgData ""
  createDirectoryIfMissing True dir
  return $ dir </> "cardano-rt-view-logs"

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

checkIfPreviousNotificationsExists :: IO (Maybe NotificationSettings)
checkIfPreviousNotificationsExists = do
  notifyExists <- savedNotificationsFile >>= doesFileExist
  if notifyExists
    then do
      config <- savedNotificationsFile >>= readNotificationsFile
      return $ Just config
    else
      return Nothing

askAboutPrevConfig
  :: Configuration
  -> RTViewParams
  -> IO (Configuration, RTViewParams)
askAboutPrevConfig savedConfig savedParams =
  TIO.getLine >>= \case
    "Y" -> return (savedConfig, savedParams)
    "y" -> return (savedConfig, savedParams)
    ""  -> return (savedConfig, savedParams)
    "N" -> startDialogToPrepareConfig
    "n" -> startDialogToPrepareConfig
    _   -> do
      colorize Red NormalIntensity $
        TIO.putStr "Sorry? <Y>es or <N>o? "
      askAboutPrevConfig savedConfig savedParams

startDialogToPrepareConfig :: IO (Configuration, RTViewParams)
startDialogToPrepareConfig = do
  colorize Magenta BoldIntensity $
    TIO.putStrLn "\nLet's configure RTView..."

  colorize Magenta NormalIntensity $
    TIO.putStr "\nPlease note that this version of RTView works with the following versions of Cardano node: "

  colorize Magenta BoldIntensity $ do
    TIO.putStr showSupportedNodesVersions
    TIO.putStr "\nPress <Enter> to continue..."
  void TIO.getLine

  colorize Green BoldIntensity $
    TIO.putStr $ "\nHow many nodes will you connect (1 - "
               <> showt maximumNode <> ", default is " <> showt defaultNodesNumber <> "): "
  nodesNumber <- askAboutNodesNumber
  let (names :: Text, nodes :: Text, are :: Text, oneAtATime :: Text)
        = if nodesNumber == 1
            then ("name",  "node",  "is",  "")
            else ("names", "nodes", "are", ", one at a time")
  TIO.putStrLn $ "Ok, " <> showt nodesNumber <> " " <> nodes <> "."

  colorize Green BoldIntensity $ do
    TIO.putStrLn ""
    TIO.putStr $ "Input the " <> names <> " of the " <> nodes <> " (default " <> are <> " \""
               <> showDefaultNodesNames nodesNumber <> "\")" <> oneAtATime <> ": "
  nodesNames <- askAboutNodesNames nodesNumber

  colorize Green BoldIntensity $ do
    TIO.putStrLn ""
    TIO.putStr $ "Indicate the port for the web server (" <> showt minimumPort
               <> " - " <> showt maximumPort <> ", default is "
               <> showt defaultRTVPort <> "): "
  port <- askAboutWebPort
  TIO.putStrLn $ "Ok, the web-page will be available on http://"
                 <> T.pack defaultRTVHost <> ":" <> showt port
                 <> ", on the machine RTView will be launched on."

  colorize Green BoldIntensity $ do
    TIO.putStrLn ""
    TIO.putStr "Indicate how your nodes should be connected with RTView: networking sockets <S> or named pipes <P>."
    TIO.putStr "\nDefault way is sockets, so if you are not sure - choose <S>: "
  (remoteAddrs, rtViewMachineHost) <- askAboutPipesAndSockets >>= \case
    Socket -> do
      TIO.putStr $ "Ok, sockets will be used. Indicate the base port to listen for connections ("
                   <> showt minimumPort <> " - " <> showt maximumPort <> ", default is "
                   <> showt defaultFirstPortForSockets <> "): "
      hFlush stdout
      addrsWithDefaultHost <- askAboutFirstPortForSockets nodesNumber
      TIO.putStr $ "Now, indicate a host of machine RTView will be launched on (default is "
                   <> T.pack defaultRTVHost <> "): "
      hFlush stdout
      host <- askAboutRTViewMachineHost
      return (addrsWithDefaultHost, host)
    Pipe -> do
      defDir <- defaultPipesDir
      TIO.putStr $ "Ok, pipes will be used. Indicate the directory for them (default is \""
                   <> T.pack defDir <> "\"): "
      hFlush stdout
      addrs <- askAboutLocationForPipes nodesNumber
      return (addrs, defaultRTVHost)

  colorize Green BoldIntensity $ do
    TIO.putStrLn ""
    TIO.putStr $ "Indicate the directory with static content for the web server (default is \""
                 <> T.pack defaultRTVStatic <> "\"): "
  staticDir <- askAboutStaticDir

  dirForLogs <- logFilesDir
  let pathToLog = T.pack (dirForLogs </> "cardano-rt-view.log")
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
                            , ScribeDefinition
                              { scName     = pathToLog
                              , scKind     = FileSK
                              , scFormat   = ScText
                              , scPrivacy  = ScPublic
                              , scMinSev   = minBound
                              , scMaxSev   = maxBound
                              , scRotation = Just $
                                  RotationParameters
                                    { rpLogLimitBytes = 50000 -- 50kB
                                    , rpMaxAgeHours   = 24
                                    , rpKeepFilesNum  = 10
                                    }
                              }
                            ]
  CM.setDefaultScribes config ["StdoutSK::stdout", ("FileSK::" <> pathToLog)]
  CM.setBackends config "cardano-rt-view.acceptor" (Just [ LogBufferBK
                                                         , UserDefinedBK "ErrorBufferBK"
                                                         ])
  let remoteAddrsNamed = map (uncurry RemoteAddrNamed)
                             $ zip nodesNames remoteAddrs
  CM.setAcceptAt config (Just remoteAddrsNamed)

  let params = defaultRTViewParams { rtvPort = port
                                   , rtvStatic = staticDir
                                   }
  -- Now show to the user the changes that should be done in node's configuration file.
  showChangesInNodeConfiguration config rtViewMachineHost

  return (config, params)

defaultNodesNumber :: Int
defaultNodesNumber = 3

maximumNode :: Int
maximumNode = 99

defaultNodeNamePrefix :: Text
defaultNodeNamePrefix = "node-"

defaultNodesNames :: Int -> [Text]
defaultNodesNames nodesNum = map (\nNum -> defaultNodeNamePrefix <> showt nNum) [1 .. nodesNum]

showDefaultNodesNames :: Int -> Text
showDefaultNodesNames nodesNumber =
  T.intercalate "\", \"" $ defaultNodesNames nodesNumber

defaultFirstPortForSockets :: Int
defaultFirstPortForSockets = 3000

defaultRTVHost :: String
defaultRTVHost = "0.0.0.0"

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
               colorize Red NormalIntensity $
                 TIO.putStr "It's not a number, please input the number instead: "
               askAboutNodesNumber
  if nodesNumber < 1 || nodesNumber > maximumNode
    then do
      colorize Red NormalIntensity $
        TIO.putStrLn $ "Wrong number of nodes, please input the number from 1 to "
                       <> showt maximumNode <> ": "
      askAboutNodesNumber
    else
      return nodesNumber

askAboutNodesNames :: Int -> IO [Text]
askAboutNodesNames nodesNumber = askNodeNames 1
 where
   askNodeNames :: Int -> IO [Text]
   askNodeNames i = do
     aName <- askNodeName i
     if | T.null aName && i == 1 -> do
            TIO.putStrLn $ "Ok, default " <> nNames <> " \"" <> showDefaultNodesNames nodesNumber
                           <> "\" will be used."
            return $ defaultNodesNames nodesNumber
        | i == nodesNumber -> do
            let theLast :: Text
                theLast = if nodesNumber == 1 then "" else "last "
            TIO.putStrLn $ "Ok, the " <> theLast <> "node has name \"" <> aName <> "\"."
            return [aName]
        | otherwise -> do
            TIO.putStr $ "Ok, node " <> showt i <> " has name \"" <> aName
                         <> "\", input the next one: "
            hFlush stdout
            names <- askNodeNames (i + 1)
            return $ aName : names

   askNodeName :: Int -> IO Text
   askNodeName i = do
    aName <- TIO.getLine
    if | T.null aName && i == 1 ->
           return ""
       | T.null aName && i /= 1 -> do
           colorize Red NormalIntensity $
             TIO.putStr "Node's name cannot be empty, please input again: "
           askNodeName i
       | T.any (== ' ') aName || T.any (== '.') aName -> do
           colorize Red NormalIntensity $
             TIO.putStr "Node's name cannot contain spaces or dots, please input again: "
           askNodeName i
       | otherwise -> return aName

   nNames :: Text
   nNames = if nodesNumber == 1 then "name" else "names"

askAboutWebPort :: IO Int
askAboutWebPort = do
  portRaw <- TIO.getLine
  port <-
    if T.null portRaw
      then return defaultRTVPort
      else case readMaybe (T.unpack portRaw) of
             Just (n :: Int) -> return n
             Nothing -> do
               colorize Red NormalIntensity $
                 TIO.putStr "It's not a number, please input the number instead: "
               askAboutWebPort
  if port < minimumPort || port > maximumPort
    then do
      colorize Red NormalIntensity $
        TIO.putStr $ "Please choose the port between " <> showt minimumPort <> " and "
                     <> showt maximumPort <> ": "
      askAboutWebPort
    else
      return port

data ConnectionWay
  = Pipe
  | Socket

askAboutPipesAndSockets :: IO ConnectionWay
askAboutPipesAndSockets =
  TIO.getLine >>= \case
    "S" -> return Socket
    "s" -> return Socket
    ""  -> return Socket
    "P" -> return Pipe
    "p" -> return Pipe
    _   -> do
      colorize Red NormalIntensity $
        TIO.putStr "Sorry? <S>ockets or <P>ipes? "
      askAboutPipesAndSockets

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
  mkpipe d = map (RemotePipe . prepname d) $
                 defaultNodesNames nodesNumber

askAboutFirstPortForSockets :: Int -> IO [RemoteAddr]
askAboutFirstPortForSockets nodesNumber = do
  firstPort <- askAboutFirstPort
  let portsForAllNodes = [firstPort .. firstPort + nodesNumber - 1]
  TIO.putStrLn $ "Ok, these ports will be used to accept nodes' metrics: " <> showPorts portsForAllNodes
  return $ map (RemoteSocket defaultRTVHost . show) portsForAllNodes
 where
  showPorts :: [Int] -> Text
  showPorts ports = T.intercalate ", " $ map showt ports

askAboutFirstPort :: IO Int
askAboutFirstPort = do
  portRaw <- TIO.getLine
  port <-
    if T.null portRaw
      then return defaultFirstPortForSockets
      else case readMaybe (T.unpack portRaw) of
             Just (n :: Int) -> return n
             Nothing -> do
               colorize Red NormalIntensity $
                 TIO.putStr "It's not a number, please input the number instead: "
               askAboutFirstPort
  if port < minimumPort || port > maximumPort
    then do
      colorize Red NormalIntensity $
        TIO.putStr $ "Please choose the port between " <> showt minimumPort <> " and "
                     <> showt maximumPort <> ": "
      askAboutFirstPort
    else
      return port

askAboutRTViewMachineHost :: IO String
askAboutRTViewMachineHost = do
  host <- T.strip <$> TIO.getLine
  if T.null host
    then do
      TIO.putStrLn "Ok, default host will be used."
      return  defaultRTVHost
    else do
      TIO.putStrLn $ "Ok, it is assumed that RTView will be launched on the host \"" <> host <> "\"."
      return $ T.unpack host

askAboutStaticDir :: IO FilePath
askAboutStaticDir = do
  dir <- T.strip <$> TIO.getLine
  if T.null dir
    then do
      TIO.putStrLn "Ok, default directory will be used."
      return defaultRTVStatic
    else do
      TIO.putStrLn $ "Ok, static content will be taken from directory \"" <> dir <> "\"."
      return $ T.unpack dir

showChangesInNodeConfiguration :: Configuration -> String -> IO ()
showChangesInNodeConfiguration config rtViewMachineHost = do
  colorize Magenta BoldIntensity $ do
    TIO.putStrLn ""
    aPath <- savedConfigurationFile
    TIO.putStr $ "Great, RTView is ready to run! Its configuration was saved at "
                 <> T.pack aPath <> ". Press <Enter> to continue..."
  void TIO.getLine
  TIO.putStrLn ""
  TIO.putStrLn "Now you have to make the following changes in your node's configuration file:"
  TIO.putStrLn ""
  enableMetricsTracing
  addTraceForwardTo
  colorize Magenta BoldIntensity $ do
    TIO.putStr "After you are done, press <Enter> to run RTView..."
  void TIO.getLine
 where
  enableMetricsTracing = do
    TIO.putStrLn "1. Find TurnOnLogMetrics flag and make sure it is true:"
    TIO.putStrLn ""
    colorize Yellow BoldIntensity $
      TIO.putStrLn "   \"TurnOnLogMetrics\": true"
    TIO.putStrLn ""

  addTraceForwardTo = do
    acceptors <- fromJust <$> CM.getAcceptAt config
    let num = length acceptors
        (nNodes :: Text, sections :: Text, its :: Text, nFiles :: Text) =
          if num == 1
            then ("1 node",              "section",  "its",   "file")
            else (showt num <> " nodes", "sections", "their", "files")
    TIO.putStrLn $ "2. Since you have "
                   <> nNodes <> ", add following traceForwardTo "
                   <> sections <> " in the root of "
                   <> its <> " configuration "
                   <> nFiles <> ":"
    TIO.putStrLn ""
    forM_ acceptors $ \(RemoteAddrNamed _ addr) -> colorize Yellow BoldIntensity $ do
      TIO.putStrLn       "   \"traceForwardTo\": {"
      case addr of
        RemoteSocket _ port -> do
          TIO.putStrLn   "     \"tag\": \"RemoteSocket\","
          TIO.putStrLn   "     \"contents\": ["
          TIO.putStrLn $ "       \"" <> T.pack rtViewMachineHost <> "\","
          TIO.putStrLn $ "       \"" <> T.pack port <> "\""
          TIO.putStrLn   "     ]"
        RemotePipe path -> do
          TIO.putStrLn   "     \"tag\": \"RemotePipe\","
#if defined(mingw32_HOST_OS)
          -- We have to escape backslashes on Windows, to avoid an error in the configuration.
          TIO.putStrLn $ "     \"contents\": \"" <> prepareForWindows (T.pack path) <> "\""
#else
          TIO.putStrLn $ "     \"contents\": \"" <> T.pack path <> "\""
#endif
      TIO.putStrLn       "   }"
      TIO.putStrLn ""

#if defined(mingw32_HOST_OS)
  -- For example, default pipe path should be displayed like this
  --
  -- "\\\\.\\pipe\\Users-Dorin-AppData-Local-Temp-_rt-view-pipes_node-1"
  --
  -- instead of
  --
  -- "\\.\pipe\Users-Dorin-AppData-Local-Temp-_rt-view-pipes_node-1"
  prepareForWindows = T.concatMap (\c -> if c == '\\' then "\\\\" else T.singleton c)
#endif

rmPipesIfNeeded :: [RemoteAddrNamed] -> IO ()
#if defined(mingw32_HOST_OS)
rmPipesIfNeeded _ = pure ()
#else
rmPipesIfNeeded acceptors = do
  let pipesDirs = map collectPipesDirs acceptors
  forM_ pipesDirs $ \dir ->
    unless (null dir) $ do
      allFiles <- listDirectory dir
      let allPipes = filter (\file -> defaultNodeNamePrefix `T.isPrefixOf` T.pack file) allFiles
      forM_ allPipes $ \pipe -> removeFile (dir </> pipe)
 where
  collectPipesDirs (RemoteAddrNamed _ (RemoteSocket _ _)) = ""
  collectPipesDirs (RemoteAddrNamed _ (RemotePipe path)) = takeDirectory path
#endif

saveConfigurationForNextSessions :: Configuration -> IO ()
saveConfigurationForNextSessions config = do
  path <- savedConfigurationFile
  CM.toRepresentation config >>= BSL.writeFile path . encodePretty

saveNotificationsForNextSessions :: NotificationSettings -> IO ()
saveNotificationsForNextSessions settings = do
  path <- savedNotificationsFile
  BSL.writeFile path $ encodePretty settings

saveRTViewParamsForNextSessions :: RTViewParams -> IO ()
saveRTViewParamsForNextSessions params = do
  path <- savedRTViewParamsFile
  BSL.writeFile path $ encodePretty params

-- | RTView requires at least one |TraceAcceptor|.
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

colorize :: Color -> ConsoleIntensity -> IO () -> IO ()
colorize color intensity action = do
  setSGR [ SetConsoleIntensity intensity
         , SetColor Foreground Vivid color
         ]
  action
  setSGR [Reset]
  hFlush stdout -- Truly resets text color to default one.

showt :: Show a => a -> Text
showt = T.pack . show
