module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, chooseInt, vectorOf)
import Data.List (minimum)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier score = (score - 10) `div` 2

ability :: Gen Int
ability = do
  dice <- vectorOf 4 (chooseInt (1,6))
  return $ sum dice - minimum dice

character :: Gen Character
character = do
  s  <- ability
  d  <- ability
  c  <- ability
  i  <- ability
  w  <- ability
  ch <- ability
  return $ Character
    { strength     = s
    , dexterity    = d
    , constitution = c
    , intelligence = i
    , wisdom       = w
    , charisma     = ch
    , hitpoints    = 10 + modifier c
    }
