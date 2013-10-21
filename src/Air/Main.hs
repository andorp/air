{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger
import Data.List (find)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.IO
import System.Log.FastLogger

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.MySQL


import Air.Domain hiding (user)
import Air.Test
import Air.Persistence
import Air.Cli.Shell

-- Config

{- Air MySQL configuration input file contains the
conection information for the mysql server.
The field names are self-descriptives -}
data Config = Config {
    host :: String
  , port :: Int
  , user :: String
  , password :: String
  , database :: String
  } deriving (Show, Eq, Read)

-- Read the configuration from the air.cfg file, that
-- contains the config in the Read instance for the
-- configuration, assuming that the configuration is
-- in the actual folder
readConfig :: IO Config
readConfig = do
  c <- readFile "air.cfg"
  return . read $ c

-- Produces a connect info for the mysql persistent layer
-- consuming the input, copying the host, etc properties
configToConnectInfo :: Config -> ConnectInfo
configToConnectInfo c = ConnectInfo {
    connectHost = host c
  , connectPort = fromIntegral $ port c
  , connectUser = user c
  , connectPassword = password c
  , connectDatabase = database c
  , connectOptions = []
  , connectPath = ""
  , connectSSL = Nothing
  }

-- Parameters

findParam :: String -> [String] -> Bool
findParam p = isJust . find (==p)

needsMigration :: [String] -> Bool
needsMigration = findParam "migrate"

-- Logger

-- Creates a logger, that opens the 'air.log' file
createLogger :: IO Logger
createLogger = openFile "air.log" AppendMode >>= mkLogger True

main = do
  config <- readConfig
  hSetBuffering stdin LineBuffering
  hSetEncoding stdin utf8

  args <- getArgs

  logger <- createLogger

  runNoLoggingT $ runResourceT $ withMySQLConn (configToConnectInfo config) $ runSqlConn $ do
--  runSqlite ":memory:" $ do

    when (needsMigration args) $ do
      runMigration migrateAll
      initPersistence

    shell logger stdin

  rmLogger logger
  hSetBuffering stdin  NoBuffering
