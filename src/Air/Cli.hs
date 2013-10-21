module Air.Cli where

-- The command that a user can give to the system. Supposint that
-- the suer parameters are allways contains active users in the system
data Command
  -- Create flatmate with a given unique username and a fullname
  = CreateFlatmate String String
  -- Deactivate flatmate with a given username
  | DeactivateFlatmate String
  -- Activate the flatmate with a given username
  | ActivateFlatmate String
  -- Create a bill with a unique name and the initial attendees, supposing that the
  -- the user list is not empty
  | CreateBill String [String]
  -- Add a new flatmate to the given bill
  | AttendBill String String
  -- Gather information about a flatmate
  | FlatmateInformation String
  -- Pay the bill with the given amount of money
  | PayBill String Int
  -- Gather the balance information of a given flatmate
  | FlatmateBalance String
  -- The given flatmate deposits some money
  | FlatmateDeposit String Int
  -- Special payment for some flatmates with a description, assuming the list is not empty
  | Payment [String] Int String
  -- Quit
  | Quit
  -- Help
  | Help
  deriving (Show, Eq)

commandCata
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
  help c = case c of
    CreateFlatmate username fullname -> createFlatmate username fullname
    DeactivateFlatmate username -> deactivateFlatmate username
    ActivateFlatmate username -> activateFlatmate username
    CreateBill billname usernames -> createBill billname usernames
    AttendBill username billname -> attendBill username billname
    FlatmateInformation username -> flatmateInformation username
    PayBill billname amount -> payBill billname amount
    FlatmateBalance username -> flatmateBalance username
    FlatmateDeposit username amount -> flatmateDeposit username amount
    Payment usernames amount desc -> payment usernames amount desc
    Quit -> quit
    Help -> help

