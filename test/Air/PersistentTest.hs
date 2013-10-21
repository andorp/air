{-# LANGUAGE OverloadedStrings #-}
module Air.PersistentTest where

import Control.Monad (when, join)
import Control.Monad.IO.Class (liftIO)

import Database.Persist
import Database.Persist.Sqlite
import System.Log.FastLogger
import System.IO (stdout)

import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit

import qualified Air.Domain      as Dom
import qualified Air.Persistence as Per

testEndToEndUser = testCase "Persistence: End to end test" $ runSqlite ":memory:" $ do
  logger <- liftIO $ mkLogger True stdout
  runMigration Per.migrateAll
  Per.initPersistence
  let andor     = Dom.User "andor"
      iboly     = Dom.User "iboly"
      krul      = Dom.User "krul"
      anette    = Dom.User "anette"
      flatmate1 = Dom.FlatMate andor "Andor"
      flatmate2 = Dom.FlatMate iboly "Iboly"
      flatmate3 = Dom.FlatMate krul  "Krul"
      flatmate4 = Dom.FlatMate anette "Anette"
  Per.insertFlatMate flatmate1
  Per.insertFlatMate flatmate2
  Per.insertFlatMate flatmate3
  checkBalance andor 0
  checkBalance iboly 0
  checkBalance krul  0
  let rent  = Dom.bill "rent"  [andor, iboly, krul]
      power = Dom.bill "power" [andor, iboly, krul]
      gas   = Dom.bill "gas"   [andor, iboly, krul]
      net   = Dom.bill "net"   [andor, krul]
      commons = Dom.bill "commons" [andor, iboly, krul]
  Per.saveBill rent
  Per.saveBill power
  Per.saveBill gas
  Per.saveBill net
  Per.saveBill commons
  checkBill "rent"    rent
  checkBill "power"   power
  checkBill "gas"     gas
  checkBill "net"     net
  checkBill "commons" commons
  Per.deposit logger andor (Dom.Deposit 50000)
  checkFlatBalance   50000
  Per.deposit logger iboly (Dom.Deposit 50000)
  checkFlatBalance  100000
  Per.deposit logger krul  (Dom.Deposit 50000)
  checkFlatBalance  150000
  checkBalance andor 50000
  checkBalance iboly 50000
  checkBalance krul  50000
  Per.payBill logger "rent" 30000
  checkFlatBalance  120000
  Per.payBill logger "net"  10000
  checkFlatBalance  110000
  checkBalance andor 35000
  checkBalance krul  35000
  checkBalance iboly 40000
  Per.insertFlatMate flatmate4
  checkBalance anette 0
  Per.deposit logger anette (Dom.Deposit 50000)
  checkBalance anette 50000
  Per.attendBill anette "rent"
  Per.payBill logger "rent" 40000
  checkBalance andor 25000
  checkBalance krul  25000
  checkBalance iboly 30000
  checkBalance anette 40000
  Per.deactivateFlatmate anette
  checkBalance andor 25000
  checkBalance krul  25000
  checkBalance iboly 30000
  checkBalance anette 0
  Per.payBill logger "rent" 30000
  checkBalance andor 15000
  checkBalance krul  15000
  checkBalance iboly 20000
  checkFlatBalance 50000
  Per.activateFlatmate anette
  checkBalance anette 0
    where
      checkBalance user n = do
        b <- Per.userBalance user
        when (Dom.balanceCata ((/=n) . floatRational) b) $
          Dom.userCata
            (\u -> fail . join $ [
                "Wrong balance: ", u, " "
              , show n, " "
              , Dom.balanceCata (show . floatRational) b
              ])
            user

      checkBill name bill = do
        b <- Per.loadBill name
        when (billUsers b /= billUsers bill) $ fail "Different users"

      checkFlatBalance n = do
        (_,bal) <- Per.loadBalance
        Dom.flatBalanceCata
          (\b -> when (floatRational b /= n) $ fail $ join [
                     "Invalied flat balance: ", show b, " ", show n
                   ])
          bal

      billUsers :: Dom.Bill -> [Dom.User]
      billUsers = Dom.billCata id id (\_ u -> u)

      floatRational :: Rational -> Float
      floatRational = fromRational
