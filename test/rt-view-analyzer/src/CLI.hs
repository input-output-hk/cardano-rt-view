{-# LANGUAGE LambdaCase #-}

module CLI
  (
    parseArguments
  )
where

import           System.Environment (getArgs)

parseArguments :: IO (FilePath, FilePath, Int)
parseArguments =
  getArgs >>= return . parseArgs >>= \case
    Left m -> putStrLn "Usage: analyzer <rt-view-config> <json-with-logobjects> <rtview-web-port>" >> error m
    Right res -> return res
 where
    parseArgs []                        = Left "No arguments"
    parseArgs (_a : [])                 = Left "Not enough arguments"
    parseArgs (_a : _b : [])            = Left "Not enough arguments"
    parseArgs (conf : json : port : []) = Right (conf, json, (read port :: Int))
    parseArgs _                         = Left "Too many arguments"
