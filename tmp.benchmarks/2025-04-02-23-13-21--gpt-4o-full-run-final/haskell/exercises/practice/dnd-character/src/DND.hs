module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)
import Data.List (sort)

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
modifier con = (con - 10) `div` 2

ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6))
  let topThree = take 3 . reverse . sort $ rolls
  return (sum topThree)

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return Character { strength = str
                   , dexterity = dex
                   , constitution = con
                   , intelligence = int
                   , wisdom = wis
                   , charisma = cha
                   , hitpoints = hp
                   }
