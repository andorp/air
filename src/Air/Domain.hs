module Air.Domain where

import Control.Monad (forM)
import Data.Map (Map)
import qualified Data.Map as Map


import Air.Test

{- | User is a string
   The user is the unique ID for the users in the database 
-}
newtype User = User String
  deriving (Show, Eq, Ord)

userCata :: (String -> a) -> User -> a
userCata f (User x) = f x

-- | FlatMate is a compound data consisting a user id and a full name
-- Interpretation: Self explanatory
data FlatMate = FlatMate {
    user     :: User
  , fullName :: String
  } deriving (Show, Eq)

flatMateCata :: (User -> a) -> (a -> String -> b) -> FlatMate -> b
flatMateCata u f (FlatMate usr name) = f (u usr) name

-- | Money is a rational number
-- Represents the current amount of money, for User's balances
-- deposits and payments.
type Money = Rational

moneyCata :: (Rational -> a) -> Money -> a
moneyCata f m = f m

-- Produces a readable string from the given amount of money
readableMoney = moneyCata (show . toFloat) where
  toFloat :: Rational -> Float
  toFloat = fromRational

-- | Balance is money
-- Interpretation: Every user has an active balance,
-- which holds the user's money
newtype Balance = Balance Money
  deriving (Show, Eq)

balanceCata :: (Money -> a) -> Balance -> a
balanceCata f (Balance x) = f x

-- | Account is a map between the user and his balance
type Account = Map User Balance

accountCata
  :: (User -> a)
  -> (Balance -> b)
  -> ((a,b) -> c)
  -> ([c] -> d)
  -> Account
  -> d
accountCata user balance pair list =
  list . map (pair . pairMap user balance) . Map.toList

pairMap f g (x,y) = (f x, g y)

-- | Deposit is an amount of money that the
-- user can add to his account.
newtype Deposit = Deposit Money

depositCata :: (Money -> a) -> Deposit -> a
depositCata f (Deposit x) = f x


-- | Payment is a compund type: The users and their ratio,
-- the total amount of money, and a description.
-- Interpretation: A payment can occur when a check arrives to the
-- flat and some of the flatmates are need to pay it, against their
-- balances, it is possible that there are differences in the weight
-- of the paymect. The ratio must be sum up to 1.0
data Payment = Payment {
    users       :: [(User, Rational)]
  , amount      :: Money
  , description :: String
  } deriving (Show, Eq)

paymentCata
  :: (User -> a)
  -> ((a, Rational) -> b)
  -> ([b] -> c)
  -> (c -> Money -> String -> d)
  -> Payment
  -> d
paymentCata user ratio list f (Payment users amount desc) =
  f (list (map (ratio . mapFst user) users)) amount desc

-- | Creates a payment from the given users, the given amount
-- of money, and a short description. The users' ratio of result payment
-- will be the same ratio
createPayment :: [User] -> Money -> String -> Payment
createPayment users amount description = Payment userAndRatioList amount description
  where
    userAndRatioList = zip users (repeat (1 / (fromIntegral (length users))))

-- | Bill is a compound type with string, users and their ratio
-- There are several types of bills can be checked to the flat
-- these bills are regular payments for the flat.
data Bill = Bill {
    billName  :: String
  , billUsers :: [User]
  } deriving (Show, Eq)

billCata
  :: (User -> a)
  -> ([a] -> c)
  -> (String -> c -> d)
  -> Bill
  -> d
billCata user list f (Bill name users) = f name (list (map user users))

-- | Creates a Payment from the given bill and the given amount of money
-- copying the money, the users and the description to the details
-- of the payment
billToPayment :: Money -> Bill -> Payment
billToPayment m = billCata id id payment
  where
    payment name users = createPayment users m name
--    userAndRatioList us = zip us (repeat (1 / (fromIntegral (length us))))

-- | Creates a bill from the given user list, supposing that the user list is not
-- empty and the users are differents
bill :: String -> [User] -> Bill
bill name us = Bill name us

-- | The FlatBalance is money
-- The total amount of money in the flat.
newtype FlatBalance = FlatBalance Money
  deriving (Show, Eq)

flatBalanceCata :: (Money -> a) -> FlatBalance -> a
flatBalanceCata f (FlatBalance x) = f x

-- * Combinators

-- | Accounting operations on different objects
class Accounting a where
  -- | Substract the given money from the account-like value
  sub :: Money -> a -> a
  -- | Add the given money to the account-like value
  add :: Money -> a -> a

instance Accounting FlatBalance where
  sub m = FlatBalance . flatBalanceCata (\x -> x - m)
  add m = FlatBalance . flatBalanceCata (\x -> x + m)

instance Accounting Balance where
  sub m = Balance . balanceCata (\x -> x - m)
  add m = Balance . balanceCata (\x -> x + m)

-- Payment

{- | Substracts the payment from the given users at the given ratio
   assuming that all the users in the payment is present in the account
   map and the given ratio is allways sum up to 1
-}
userPayment :: Payment -> Account -> Account
userPayment p a = paymentCata id id id split p
  where
    split :: [(User, Rational)] -> Money -> String -> Account
    split users money _ =
      let userPayments = map (\(user,ratio) -> (user, (money * ratio))) users
          subFromUser m (user, payment) = Map.update (Just . sub payment) user m
      in foldl subFromUser a userPayments

{- | The total amount of the money substracted from the balance of the flat-}
flatPayment :: Payment -> FlatBalance -> FlatBalance
flatPayment p = sub (amount p)

payment :: Payment -> (Account, FlatBalance) -> (Account, FlatBalance)
payment p (account, flat) = (userPayment p account, flatPayment p flat)

deactivateUser :: User -> Balance -> Payment
deactivateUser u b = Payment {
    users  = [(u, 1)]
  , amount = balanceCata id b
  , description = userCata (\u -> "The deactivation of the user:" ++ u) u
  }

-- Deposit

{- | Adds a deposit for the user supposing that the user is
   in the account list
-}
userDeposit :: User -> Deposit -> Account -> Account
userDeposit u d = Map.update f u
  where f = Just . add (depositCata id d)

{-| Add some money to the total account of the flat -}
flatDeposit :: Deposit -> FlatBalance -> FlatBalance
flatDeposit d = add (depositCata id d)

deposit :: User -> Deposit -> (Account, FlatBalance) -> (Account, FlatBalance)
deposit user deposit (account, flat) = (userDeposit user deposit account, flatDeposit deposit flat)

-- * Tools

mapFst f (x,y) = (f x, y)

-- * Tests

flatDepositTest0 = Equals (FlatBalance 10) (flatDeposit (Deposit 2) (FlatBalance 8))  "Flat Deposit 2"
flatDepositTest1 = Equals (FlatBalance 10) (flatDeposit (Deposit 0) (FlatBalance 10)) "Flat Deposit 0"
flatDepositTest2 = Equals (FlatBalance 10) (flatDeposit (Deposit (-2)) (FlatBalance 12)) "Flat Deposit -2"

pym :: Money -> Payment
pym m = Payment [] m ""

flatPaymentTest0 = Equals (FlatBalance 10) (flatPayment (pym 2) (FlatBalance 12)) "Flat Payment 2"
flatPaymentTest1 = Equals (FlatBalance 10) (flatPayment (pym 0) (FlatBalance 10)) "Flat Payment 0"
flatPaymentTest2 = Equals (FlatBalance 10) (flatPayment (pym (-2)) (FlatBalance 8)) "Flat Payment -2"

userDepositTest0 = Equals
  (Map.fromList [(User "u", Balance 10)])
  (userDeposit (User "u") (Deposit 2) (Map.fromList [(User "u", Balance 8)]))
  "User Deposit 2"

userDepositTest1 = Equals
  (Map.fromList [(User "u", Balance 10)])
  (userDeposit (User "u") (Deposit 0) (Map.fromList [(User "u", Balance 10)]))
  "User Deposit 0"

userDepositTest2 = Equals
  (Map.fromList [(User "u", Balance 10)])
  (userDeposit (User "u") (Deposit (-2)) (Map.fromList [(User "u", Balance 12)]))
  "User Deposit -2"

userPaymentTest0 = Equals
  (Map.fromList [(User "u1", Balance 10), (User "u2", Balance 100), (User "u3", Balance 20)])
  (userPayment
    (Payment [(User "u1", 0.6), (User "u2", 0.4)] 100 "")
    (Map.fromList [(User "u1", Balance 70), (User "u2", Balance 140), (User "u3", Balance 20)]))
  "User Payment 100"

userPaymentTest1 = Equals
  (Map.fromList [(User "u1", Balance 10), (User "u2", Balance 100), (User "u3", Balance 20)])
  (userPayment
    (Payment [(User "u1", 0.6), (User "u2", 0.4)] 0 "")
    (Map.fromList [(User "u1", Balance 10), (User "u2", Balance 100), (User "u3", Balance 20)]))
  "User Payment 0"

billToPaymentTest0 = Equals
  (Payment [] 0 "") (billToPayment 0 (Bill "" [])) "Empty users and description"

billToPaymentTest1 = Equals
  (Payment [(User "u1", 0.5), (User "u2", 0.5)] 100 "desc")
  (billToPayment 100 (Bill "desc" [(User "u1"), (User "u2")]))
  "Non empty user description"

billTest0 = Equals
  (Bill "d" [(User "a"), (User "b")])
  (bill "d" [User "a", User "b"])
  "Same ratio to everyone bill test"
