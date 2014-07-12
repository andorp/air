{-# LANGUAGE OverloadedStrings #-}
module Air.Cli.Parser where

import Control.Applicative ((<$>), (<*>), (<*))

import Data.Char (chr)
import Data.ByteString hiding (map, elem)
import Data.Attoparsec hiding (satisfy)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8

import Air.Cli (Command(..))
import qualified Air.Domain as D

import Air.Test

parseCommand :: ByteString -> Either String Command
parseCommand = parseOnly command

command :: Parser Command
command = choice [
    createFlatmate
  , deactivateFlatmate
  , activateFlatmate
  , createBill
  , attendBill
  , flatmateInformation
  , payBill
  , flatmateBalance
  , flatmateDeposit
  , payment
  , quit
  , help
  ]

-- eg parses the "quit"
quit :: Parser Command
quit = string "quit" >> return Quit

-- eg parses the "help"
help :: Parser Command
help = string "help" >> return Help

-- eg parses the  "create flatmate username Bob Marley"
createFlatmate :: Parser Command
createFlatmate = do
  cmd "create" "flatmate"
  CreateFlatmate
    <$> (username <* spaces)
    <*> (many1 (letter <|> space))

-- eg parses the "deactivate flatmate username"
deactivateFlatmate :: Parser Command
deactivateFlatmate = do
  cmd "deactivate" "flatmate"
  DeactivateFlatmate <$> username

-- eg parses the "activate flatmate username"
activateFlatmate :: Parser Command
activateFlatmate = do
  cmd "activate" "flatmate"
  ActivateFlatmate <$> username

-- eg parses the "create bill billname [usera,userb]"
createBill :: Parser Command
createBill = do
  cmd "create" "bill"
  CreateBill
    <$> (billname <* spaces)
    <*> (listOf username)

-- eg parses the "attend bill user billname"
attendBill :: Parser Command
attendBill = do
  cmd "attend" "bill"
  AttendBill
    <$> (username <* spaces)
    <*> billname

-- eg parses the "information username"
flatmateInformation :: Parser Command
flatmateInformation = do
  spacesAfter $ string "information"
  FlatmateInformation <$> username

-- eg parses the "pay bill billname {number}"
payBill :: Parser Command
payBill = do
  string "pay"
  spaces
  PayBill
    <$> (billname <* spaces)
    <*> decimal

-- eg parses the "flatmate balance username"
flatmateBalance :: Parser Command
flatmateBalance = do
  cmd "flatmate" "balance"
  FlatmateBalance <$> username

-- eg parses the "flatmate deposit username {number}"
flatmateDeposit :: Parser Command
flatmateDeposit =
  FlatmateDeposit
    <$> (username <* spaces <* string "deposits" <* spaces)
    <*> decimal

-- eg parses the "payment [usera, userb] {number} description"
payment :: Parser Command
payment = do
  spacesAfter $ string "payment"
  Payment
    <$> ((listOf username) <* spaces)
    <*> (decimal <* spaces)
    <*> (many1 (letter <|> space))

username = many1 letter
billname = many1 letter

cmd f s = do
  f
  spaces
  s
  spaces

-- Tools

spacesAfter parser = do
  x <- parser
  spaces
  return x

brackets open parser close = do
  open
  x <- parser
  close
  return x

listOf p = brackets (char '[') (p `sepBy` (char ',' >> spaces)) (char ']')

spaces = many1 space
letter = accented_letter <|> letter_ascii

accented_letter = do
  satisfy (flip elem (map chr [
      225, 237, 369, 337, 252, 246, 250, 243, 233
    , 193, 205, 368, 336, 220, 214, 218, 211, 201
    ]))


a <|> b = choice [a,b]
{-# INLINE (<|>) #-}

-- Test
parserTests = [
    Equals (parseCommand "create flatmate  user Bob User")
           (Right (CreateFlatmate "user" "Bob User"))
           "Parser Test: Create Flatmate"

  , Equals (parseCommand "deactivate flatmate user")
           (Right (DeactivateFlatmate "user"))
           "Parser Test: Deactivate Flatmate"

  , Equals (parseCommand "activate flatmate user")
           (Right (ActivateFlatmate "user"))
           "Parser Test: Activate Flatmate"

  , Equals (parseCommand "create bill billname [usera, userb, userc]")
           (Right (CreateBill "billname" ["usera", "userb", "userc"]))
           "Parser Test: Create Bill"

  , Equals (parseCommand "attend bill user bill")
           (Right (AttendBill "user" "bill"))
           "Parser Test: Attend Bill"

  , Equals (parseCommand "information user")
           (Right (FlatmateInformation "user"))
           "Parser Test: Flatmate Information"

  , Equals (parseCommand "pay common 10000")
           (Right (PayBill "common" 10000))
           "Parser Test: Pay Bill"

  , Equals (parseCommand "flatmate balance user")
           (Right (FlatmateBalance "user"))
           "Parser Test: Flatmate Balance"

  , Equals (parseCommand "user deposits 10000")
           (Right (FlatmateDeposit "user" 10000))
           "Parser Test: Flatmate Deposit"

  , Equals (parseCommand "payment [usera, userb] 100 This is a description")
           (Right (Payment ["usera", "userb"] 100 "This is a description"))
           "Parser Test: Payment"

  , Equals (parseCommand "quit") (Right Quit) "Parses Test: Quit"
  , Equals (parseCommand "help") (Right Help) "Parses Test: Help"
  ]
