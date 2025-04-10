module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)

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

-- Calculate the ability modifier
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- Roll four 6-sided dice, drop the lowest, sum the rest
ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1,6))
  let topThree = sum $ take 3 $ reverse $ quicksort rolls
  return topThree
  where
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, y < x]
                       ++ [x] ++
                       quicksort [y | y <- xs, y >= x]

-- Generate a random character
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
