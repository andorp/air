{-# LANGUAGE OverloadedStrings #-}
module Air.Logger where

import Prelude hiding (log)
import Control.Monad (forM_, join)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Time (UTCTime, getCurrentTime)
import System.Log.FastLogger

import Air.Domain

username = userCata id

balance = balanceCata readableMoney

depositMsg = depositCata readableMoney

payment :: Payment -> [LogStr]
payment = paymentCata username id id logLines where
  logLines rationals money name = header:(map logLine rationals) where
    header = toLogStr . join $ ["The ", name, " is payed with ", readableMoney money]
    logLine (user, ratio) = toLogStr . join $ [
        user, " pays ", readableMoney $ (ratio * money)
      , " for ", name, "."
      ]

account :: Account -> [LogStr]
account = accountCata username balance logLine id where
  logLine (name,bal) = toLogStr . join $ [name, " has ", bal]

flatBalance :: FlatBalance -> LogStr
flatBalance = flatBalanceCata $ \b -> toLogStr . join $ ["The total amount is: ", readableMoney b]

deposit :: User -> Deposit -> LogStr
deposit u d = toLogStr . join $ [username u, " deposits ", depositMsg d]

-- Logs the given log message attaching the actual time-stamp and the newline character
-- at the end of the string
log :: LoggerSet -> [LogStr] -> IO ()
log logger lines = do
  now <- getCurrentTime
  forM_ lines $ \line -> do
    pushLogStr logger . toLogStr $ concat ["[", show now, "] "]
    pushLogStr logger line
    pushLogStr logger $ toLogStr ("\n" :: String)

-- Logs a single line log message attaching the actual time-stamp and the newline character
-- at the end of the string
log' :: LoggerSet -> LogStr -> IO ()
log' l m = log l [m]
