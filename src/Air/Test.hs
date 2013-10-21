{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Air.Test where

-- The Message is a string
type Message = String

data Assertion a
  = Equals    a a Message
  | NotEquals a a Message
  | Predicate (a -> Bool) a Message

assertionCata
  :: (a -> a -> Message -> b)
  -> (a -> a -> Message -> b)
  -> ((a -> Bool) -> a -> Message -> b)
  -> Assertion a
  -> b
assertionCata f _ _ (Equals x y m)    = f x y m
assertionCata _ f _ (NotEquals x y m) = f x y m
assertionCata _ _ f (Predicate g x m) = f g x m
