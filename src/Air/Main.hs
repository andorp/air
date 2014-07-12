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

import Air.Domain hiding (user)
import Air.Test
import Air.Persistence
import Air.Cli.Shell

-- Config

-- Parameters

findParam :: String -> [String] -> Bool
findParam p = isJust . find (==p)

needsMigration :: [String] -> Bool
needsMigration = findParam "migrate"

-- Logger

-- Creates a logger, that opens the 'air.log' file
createLogger :: IO LoggerSet
createLogger = newFileLoggerSet defaultBufSize "air.log"

main = do
  hSetBuffering stdin LineBuffering
  hSetEncoding stdin utf8

  args <- getArgs

  logger <- createLogger

  runSqlite "air.db" $ do

    when (needsMigration args) $ do
      runMigration migrateAll
      initPersistence

    shell logger stdin

  rmLoggerSet logger
  hSetBuffering stdin  NoBuffering
