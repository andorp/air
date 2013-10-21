module Main where

import qualified Test.HUnit as HUnit
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.Framework
import Test.Framework.Providers.HUnit

import Air.Domain
import Air.Test
import Air.PersistentTest
import Air.Cli.Parser
import Air.Cli.Shell

test :: (Show a, Eq a) => Assertion a -> Test
test = assertionCata equals notEquals predicate
  where
    equals x y m    = testCase m $ HUnit.assertBool ("Failed! Found: " ++ show x ++ " Expected: " ++ show y) (x == y)
    notEquals x y m = testCase m $ HUnit.assertBool "Failed" (x /= y)
    predicate f x m = testCase m $ HUnit.assertBool "Failed" (f x)

tests = [
    test flatDepositTest0
  , test flatDepositTest1
  , test flatDepositTest2
  , test flatPaymentTest0
  , test flatPaymentTest1
  , test flatPaymentTest2
  , test userDepositTest0
  , test userDepositTest1
  , test userDepositTest2
  , test userPaymentTest0
  , test userPaymentTest1
  , test billToPaymentTest0
  , test billToPaymentTest1
  , test billTest0
  , testEndToEndUser
  ] ++
  map test parserTests ++
  map test shellTests

main = defaultMain tests
