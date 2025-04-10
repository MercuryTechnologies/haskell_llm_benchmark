module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, chooseInt, sample')

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

-- Function to roll four dice, discard the lowest, and sum the rest
rollFourDice :: Gen Int
rollFourDice = do
  rolls <- mapM (\_ -> chooseInt (1, 6)) [1..4]
  return $ sum $ take 3 $ reverse $ sort rolls

-- Generate an ability score
ability :: Gen Int
ability = rollFourDice

-- Calculate the modifier for a given ability score
modifier :: Int -> Int
modifier n = (n - 10) `div` 2

-- Generate a character
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  intel <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return $ Character str dex con intel wis cha hp

-- Helper function to sort a list
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]
