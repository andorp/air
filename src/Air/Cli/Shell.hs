module Air.Cli.Shell where

import Control.Monad (liftM, forM, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (maybe)
import Data.Map (Map)
import qualified Data.Map as Map

import Database.Persist
import System.IO (Handle, hGetLine)
import System.Log.FastLogger (LoggerSet)

import Air.Cli (Command(..), commandCata)
import Air.Cli.Interpretation
import Air.Cli.Parser
import Air.Domain hiding (Payment)
import Air.Test

-- Print Read Eval Loop
shell :: (PersistQuery m) => LoggerSet -> Handle -> m ()
shell logger handler = do
  context <- storedContext
  printContext context
  printPrompt
  line <- read
  case parseCommand line of
    Right Quit -> return ()
    Right Help ->
      do printHelp
         shell logger handler
    Right cmd ->
      do case semanticalCheck context cmd of
           True  -> interpretation logger cmd >>= resultCata noResult printError flatmateInfo
           False -> liftIO $ putStrLn "Wrong username, billname, or amount was used."
         shell logger handler
    Left  err ->
      do printError err
         shell logger handler
  where
    read = liftIO $ B.hGetLine handler

    noResult = return ()
    flatmateInfo _ = return ()

printPrompt :: (PersistQuery m) => m ()
printPrompt = liftIO $ putStr "> "

printContext :: (PersistQuery m) => Context -> m ()
printContext context = do
  flip contextCata context $ \users bills balances flatBalance -> liftIO $ do
    putStrLn "Users:"
    putStrLn "======"
    forM users $ \user -> do
      userCata putStr user
      putStr " "
      putStrLn $ maybe "???" (balanceCata showRational) (Map.lookup user balances)
    putStrLn "Bills:"
    putStrLn "======"
    forM bills putStrLn
    putStrLn "FlatBalance:"
    putStrLn "============"
    flatBalanceCata (putStrLn . showRational) flatBalance
  return ()
  where
    showRational :: Rational -> String
    showRational = show . fromRational

printError :: (PersistQuery m) => String -> m ()
printError e = do
  liftIO . putStrLn $ e
  return ()

void m = m >> return ()

printHelp :: (PersistQuery m) => m ()
printHelp = liftIO $ do
  putStrLn "List of commands:"
  putStrLn "quit"
  putStrLn "help"
  putStrLn "create flatmate username full name"
  putStrLn "deactivate flatmate username"
  putStrLn "activate flatmate username"
  putStrLn "create bill billname [usera,userb]"
  putStrLn "attend bill user billname"
  putStrLn "information username"
  putStrLn "pay billname {number}"
  putStrLn "flatmate balance username"
  putStrLn "username deposits {number}"
  putStrLn "payment [usera, userb] {number} description"

-- Checks if the given usernames, bill names in the command parameter
-- are in the current context. The amounts must be greater than zero
semanticalCheck :: Context -> Command -> Bool
semanticalCheck context command =
  flip contextCata context $ \users bills balances flatBalance ->
    let validUser username = elem (User username) users
        validBill billname = elem billname bills
        validUsers = and . (map validUser)
    in commandCata
         (\username _ -> not $ validUser username) -- createFlatmate
         (\username   -> validUser username) -- deactivateFlatmate
         (const True) -- activateFlatmate
         (\billname usernames -> (not (validBill billname)) && validUsers usernames) -- createBill
         (\username billname  -> validBill billname && validUser username)   -- attendBill
         (\username -> validUser username) -- flatmateInformation
         (\billname amount  -> validBill billname && (amount > 0)) -- payBill
         (\username -> validUser username) -- flatmateBalance
         (\username amount -> validUser username && (amount > 0)) -- flatmateDeposit
         (\usernames amount _ -> validUsers usernames && (amount > 0)) -- payment
         True -- quit
         True -- help
         command

testUsers = map User ["a", "i", "s"]
testBills = ["electricity", "common", "flat", "gas"]
testContext1 = Context
  ( testUsers
  , testBills
  , Map.fromList (testUsers `zip` (repeat (Balance 0)))
  , FlatBalance 0
  )

-- TODO
shellTests = [
    Equals (semanticalCheck testContext1 (CreateFlatmate "b" "b")) True  "Create new flatmate"
  , Equals (semanticalCheck testContext1 (CreateFlatmate "a" "a")) False "Create existing flatmate "
  , Equals (semanticalCheck testContext1 (DeactivateFlatmate "a")) True "Deactivate flatmate"
  , Equals (semanticalCheck testContext1 (DeactivateFlatmate "c")) False "Deactivate non-existing flatmate"
    -- Allways True handled by the persistent layer
  , Equals (semanticalCheck testContext1 (ActivateFlatmate "a")) True "Activate flatmate"
  , Equals (semanticalCheck testContext1 (CreateBill "b" ["a", "i", "s"])) True "Create new bill with existing users"
  , Equals (semanticalCheck testContext1 (CreateBill "b" ["a", "c", "s"])) False "Create new bill with non-existing users"
  , Equals (semanticalCheck testContext1 (CreateBill "common" ["a", "i", "s"])) False "Create existing bill with existing users"
  , Equals (semanticalCheck testContext1 (AttendBill "a" "common")) True "User attends in an exsiting bill"
  , Equals (semanticalCheck testContext1 (AttendBill "a" "b")) False "User attends in a non exsiting bill"
  , Equals (semanticalCheck testContext1 (FlatmateInformation "a")) True "Information about an existing user"
  , Equals (semanticalCheck testContext1 (FlatmateInformation "g")) False "Information about a non-existing user"
  , Equals (semanticalCheck testContext1 (PayBill "flat" 10000)) True "Pay an existing bill"
  , Equals (semanticalCheck testContext1 (PayBill "flat" 0)) False "Pay an existing bill with 0 money"
  , Equals (semanticalCheck testContext1 (PayBill "flat" (-200))) False "Pay an existing bill with 0 money"
  , Equals (semanticalCheck testContext1 (PayBill "a" 0)) False "Pay a non-existing bill with 0 money"

  , Equals (semanticalCheck testContext1 (FlatmateBalance "a")) True "Get the balance information about an existing flatmate"
  , Equals (semanticalCheck testContext1 (FlatmateBalance "v")) False "Get the balance information about a non-existing flatmate"

  , Equals (semanticalCheck testContext1 (FlatmateDeposit "a" 10000)) True "Existing flatmate deposits 10000"
  , Equals (semanticalCheck testContext1 (FlatmateDeposit "a" 0)) False "Existing flatmate deposits 0"
  , Equals (semanticalCheck testContext1 (FlatmateDeposit "a" (-100))) False "Existing flatmate deposits 100"
  , Equals (semanticalCheck testContext1 (FlatmateDeposit "v" 100)) False "Non-existing flatmate deposits 100"

  , Equals (semanticalCheck testContext1 (Payment ["a", "i", "s"] 10000 "desc")) True "Payment with existing users and valid amount"
  , Equals (semanticalCheck testContext1 (Payment ["a", "v", "s"] 10000 "desc")) False "Payment with non-existing users and valid amount"
  , Equals (semanticalCheck testContext1 (Payment ["a", "i", "s"] (-100) "desc")) False "Payment with existing users and non-valid amount"
  , Equals (semanticalCheck testContext1 (Payment ["a", "i", "s"] 0 "desc")) False "Payment with existing users and non-valid amount"

  , Equals (semanticalCheck testContext1 Quit) True "Quit is allways valid"
  , Equals (semanticalCheck testContext1 Help) True "Help is allways valid"
  ]
