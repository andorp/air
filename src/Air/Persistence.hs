{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
module Air.Persistence where

import Control.Monad (forM, forM_, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Log.FastLogger

import qualified Air.Domain as D
import qualified Air.Logger as L

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

FlatMate
  user   String
  name   String
  active Bool
  User   user
  deriving Show

Balance
  balance Rational
  userId  FlatMateId
  UniqueBalance userId
  deriving Show

FlatBalance
  flatBalance Rational
  deriving Show

Deposit
  deposit Rational
  userId  FlatMateId
  date    UTCTime
  deriving Show

Bill
  bill String
  UniqueBill bill
  deriving Show

BillAttendee
  billId BillId
  userId FlatMateId
  UniqueBillUsers billId userId

Payment
  payment     Rational
  description String
  date        UTCTime

|]

initPersistence :: PersistQuery m => m ()
initPersistence = do
  flatBalance <- selectFlatBalance
  case flatBalance of
    [] -> void $ insert (FlatBalance 0)
    _ -> return ()
  where
    selectFlatBalance = do
      less <- selectList [FlatBalanceFlatBalance <. 0] []
      greater <- selectList [FlatBalanceFlatBalance >=. 0] []
      return (less ++ greater)

saveBill :: PersistQuery m => D.Bill -> m ()
saveBill = D.billCata username id save
  where
    username = D.userCata id
    save name users = do
      bill <- insert $ Bill name
      forM_ users $ \username -> do
        [flatmate] <- selectList [FlatMateUser ==. username] []
        insert $ BillAttendee bill (entityKey flatmate)

{- | Returns a bill for the given unique name supposing that the bill exist in the database
the bill contains only the active users
-}
loadBill :: PersistQuery m => String -> m D.Bill
loadBill name = do
  [b] <- selectList [BillBill ==. name] []
  flatmates <- for (selectList [BillAttendeeBillId ==. (entityKey b)] []) $ \attendee ->
                 getJust . billAttendeeUserId . entityVal $ attendee
  let activeUsers = map (D.User . flatMateUser) . filter flatMateActive $ flatmates
  return $ D.Bill name activeUsers

{- Make attend a user for a given bill if the user is not attending the bill.
assuming that the given user and the given bill exist in the persistent layer
-}
attendBill :: (PersistQuery m) => D.User -> String -> m ()
attendBill user bill = do
  [flatmate] <- selectList [FlatMateUser ==. (D.userCata id user)] []
  [bill] <- selectList [BillBill ==. bill] []
  let billId = entityKey bill
      flatmateId = entityKey flatmate
  billAttendees <- selectList [BillAttendeeBillId ==. billId, BillAttendeeUserId ==. flatmateId ] []
  case billAttendees of
    [] -> void $ insert $ BillAttendee (entityKey bill) (entityKey flatmate)
    _  -> return ()
  return ()

-- | Calculates a list the list for the given user no mather if
-- the user is deactivated
flatmateBills :: PersistQuery m => D.User -> m [String]
flatmateBills = D.userCata $ \user -> do
  [flatmate] <- selectList [FlatMateUser ==. user] []
  billAttendees <- selectList [BillAttendeeUserId ==. (entityKey flatmate)] []
  let billIds = nub . map (billAttendeeBillId . entityVal) $ billAttendees
  forM billIds (liftM billBill . getJust)

payBill :: PersistQuery m => LoggerSet -> String -> Rational -> m ()
payBill logger name amount = do
  bill <- loadBill name
  let p = D.billToPayment amount bill
  liftIO $ L.log logger (L.payment p)
  payment p

users :: PersistQuery m => m [D.User]
users = liftM (map (D.User . flatMateUser . entityVal)) (selectList [FlatMateActive ==. True] [])

bills :: PersistQuery m => m [String]
bills = liftM (map (billBill . entityVal)) (selectList [] [])

{- Saves the flatmate and creates a balance with 0 value -}
insertFlatMate :: PersistQuery m => D.FlatMate -> m ()
insertFlatMate flatmate = do
  user <- D.flatMateCata (D.userCata id) (\u f -> insert $ FlatMate u f True) flatmate
  insert $ Balance 0 user
  return ()

updateAccounts :: PersistQuery m => D.Account -> m ()
updateAccounts accounts = do
  forM_ (Map.keys accounts) $ \u -> do
    [flatmate]    <- selectList [FlatMateUser  ==. (D.userCata id u)]    []
    [userBalance] <- selectList [BalanceUserId ==. (entityKey flatmate)] []
    let newBalance = D.balanceCata id . fromJust . Map.lookup u $ accounts
    update (entityKey userBalance) [ BalanceBalance =. newBalance ]

updateFlatBalance :: PersistQuery m => D.FlatBalance -> m ()
updateFlatBalance = D.flatBalanceCata $ \b -> do
  [balance] <- selectList [] []
  update (entityKey balance) [FlatBalanceFlatBalance =. b ]

updateBalance :: PersistQuery m => (D.Account, D.FlatBalance) -> m ()
updateBalance (accounts, flatBalance) = do
  updateAccounts accounts
  updateFlatBalance flatBalance

loadBalance :: PersistQuery m => m (D.Account, D.FlatBalance)
loadBalance = do
  users <- selectList [FlatMateActive ==. True] []
  userBalances <- forM users $ \u -> do
    [b] <- selectList [BalanceUserId ==. (entityKey u)] []
    return ( D.User    . flatMateUser   . entityVal $ u
           , D.Balance . balanceBalance . entityVal $ b
           )
  [flat] <- selectList [] []
  return ( Map.fromList userBalances
         , D.FlatBalance . flatBalanceFlatBalance . entityVal $ flat
         )

userBalance :: PersistQuery m => D.User -> m D.Balance
userBalance = D.userCata $ \u -> do
  [user] <- selectList [ FlatMateUser ==. u ] []
  [b] <- selectList [ BalanceUserId ==. (entityKey user) ] []
  return . D.Balance . balanceBalance . entityVal $ b

deposit :: PersistQuery m => LoggerSet -> D.User -> D.Deposit -> m ()
deposit logger u d = do
  balance <- loadBalance
  updateBalance (D.deposit u d balance)
  insertDeposit
  where
    insertDeposit = do
      now <- liftIO $ getCurrentTime
      [user] <- selectList [ FlatMateUser ==. (D.userCata id u)] []
      let deposit = Deposit (D.depositCata id d) (entityKey user) now
      liftIO $ L.log' logger (L.deposit u d)
      insert deposit
      return ()

logBalance :: PersistQuery m => LoggerSet -> m ()
logBalance logger = do
  b <- loadBalance
  let (a,f) = D.pairMap L.account L.flatBalance b
  liftIO $ L.log logger (f:a)

payment :: PersistQuery m => D.Payment -> m ()
payment p = do
  balance <- loadBalance
  updateBalance (D.payment p balance)
  insertPayment p
  where
    insertPayment = D.paymentCata id id id $ \_ money desc -> do
      now <- liftIO $ getCurrentTime
      insert $ Payment money desc now
      return ()

inactiveFlatmates :: PersistQuery m => m [D.User]
inactiveFlatmates = do
  fs <- selectList [ FlatMateActive ==. False ] []
  return (map (D.User . flatMateUser . entityVal) fs)

{- The flatmate can be deactivated, after the deactivitation his balance
   changes to zero, and his money substracted from the flatbalance as well -}
deactivateFlatmate :: PersistQuery m => D.User -> m ()
deactivateFlatmate user = do
  b <- userBalance user
  payment (D.deactivateUser user b)
  [flatmate] <- selectList [FlatMateUser ==. (D.userCata id user)] []
  update (entityKey flatmate) [FlatMateActive =. False]

activateFlatmate :: PersistQuery m => D.User -> m ()
activateFlatmate user = do
  [flatmate] <- selectList [FlatMateUser ==. (D.userCata id user)] []
  update (entityKey flatmate) [FlatMateActive =. True]

-- * Helpers

for :: (Monad m) => m [a] -> (a -> m b) -> m [b]
for m f = m >>= (mapM f)

void :: (Monad m) => m a -> m ()
void m = m >> return ()
