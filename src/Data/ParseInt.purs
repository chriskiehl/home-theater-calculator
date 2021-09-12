-- | Basically stolen from purescript-parseint[0] because 
-- | I can't figure out how to add something to my Spago 
-- | file which isn't found in the packages.dhall repo 
-- | [0] https://github.com/athanclark/purescript-parseint/blob/master/src/Data/Int/Parse.purs
module ParseInt where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)

type Radix = Number 

foreign import unsafeParseInt :: Fn2 String Radix Number   

parseInt :: String -> Radix -> Maybe Int 
parseInt s r = 
  let x = runFn2 unsafeParseInt s r 
  in if isNaN x then Nothing else Just (round x)

parseBase10 :: String -> Maybe Int 
parseBase10 s = if s == ""
  then Just 0 
  else parseInt s 10.0
