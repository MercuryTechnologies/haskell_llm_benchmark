module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)
-- Removed unused import: import Control.Applicative (liftA6)
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

-- Calculates the modifier for a given ability score.
-- The formula is: floor((score - 10) / 2)
modifier :: Int -> Int
modifier score = floor $ (fromIntegral score - 10) / 2

-- Generates a single ability score.
-- Rolls 4d6, drops the lowest roll, and sums the remaining three.
ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6))
  return $ sum (drop 1 (sort rolls))

-- Generates a complete character.
-- Generates six ability scores using the `ability` generator.
-- Calculates hitpoints based on the constitution modifier.
character :: Gen Character
character = do
    str <- ability
    dex <- ability
    con <- ability
    int <- ability
    wis <- ability
    cha <- ability
    let hp = 10 + modifier con
    return $ Character str dex con int wis cha hp
