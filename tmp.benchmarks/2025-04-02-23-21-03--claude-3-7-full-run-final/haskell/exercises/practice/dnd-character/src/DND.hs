module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (choose, vectorOf)
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

-- Calculate the ability modifier by subtracting 10, dividing by 2 and rounding down
modifier :: Int -> Int
modifier ability = (ability - 10) `div` 2

-- Generate a random ability score by rolling 4d6 and taking the sum of the highest 3
ability :: Gen Int
ability = do
  dice <- vectorOf 4 (choose (1, 6))
  return $ sum $ take 3 $ reverse $ sort dice

-- Generate a random character with abilities and hitpoints
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return Character
    { strength = str
    , dexterity = dex
    , constitution = con
    , intelligence = int
    , wisdom = wis
    , charisma = cha
    , hitpoints = hp
    }
