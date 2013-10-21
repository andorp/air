module Air.Cli.Interpretation where

import Database.Persist
import System.Log.FastLogger (Logger)

import Air.Cli
import qualified Air.Domain      as D
import qualified Air.Persistence as P

-- The persistence layer interpretation of a given command. A branch is selected
-- for the given command, and with the result, the continuation computation
-- is called.
interpretation :: (PersistQuery m) => Logger -> Command -> m Result
interpretation logger = commandCata
  createFlatmate
  deactivateFlatmate
  activateFlatmate
  createBill
  attendBill
  flatmateInformation
  payBill
  flatmateBalance
  flatmateDeposit
  payment
  quit
  help
  where
    createFlatmate username fullname
      = do P.insertFlatMate (D.FlatMate (D.User username) fullname)
           return NoResult

    deactivateFlatmate username
      = do P.deactivateFlatmate (D.User username)
           return NoResult

    activateFlatmate username
      = do let user = D.User username
           inactives <- P.inactiveFlatmates
           case elem user inactives of
             True -> do P.activateFlatmate user
                        return NoResult
             False -> return $ ErrResult $ "Not an inactive user" ++ show inactives

    createBill billname usernames
      = do P.saveBill (D.Bill billname (map D.User usernames))
           return NoResult

    attendBill username billname
      = do P.attendBill (D.User username) billname
           return NoResult

    flatmateInformation username
      = undefined

    payBill billname amount
      = do P.logBalance logger
           P.payBill logger billname (fromIntegral amount)
           P.logBalance logger
           return NoResult

    flatmateBalance username
      = do P.userBalance (D.User username)
           return NoResult

    flatmateDeposit username amount
      = do P.logBalance logger
           P.deposit logger (D.User username) (D.Deposit (fromIntegral amount))
           P.logBalance logger
           return NoResult

    payment usernames amount desc
      = do P.logBalance logger
           P.payment (D.createPayment (map D.User usernames) (fromIntegral amount) desc)
           P.logBalance logger
           return NoResult

    quit = return NoResult

    help = return NoResult

-- The result of the interpreted command
data Result
  = NoResult -- No additional information is computed
  | ErrResult String -- Some error happened, with a given message
  | FlatmateInfo D.FlatMate -- The description of the flatmate
  deriving (Show, Eq)

resultCata noResult errResult flatmateInfo r = case r of
  NoResult -> noResult
  ErrResult msg -> errResult msg
  FlatmateInfo flatmate -> flatmateInfo flatmate

-- Computational context containing users, bills, balances and the flatbalance.
newtype Context = Context ([D.User], [String], D.Account, D.FlatBalance)
  deriving (Show, Eq)

contextCata f (Context (users, bills, balances, flatBalance)) =
  f users bills balances flatBalance

-- Read the stored context from the persistent layer
storedContext :: (PersistQuery m) => m Context
storedContext = do
  us <- P.users
  bs <- P.bills
  (balance, flat) <- P.loadBalance
  return (Context (us, bs, balance, flat))
