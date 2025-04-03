module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)
import Data.List (sort)
-- Import liftA6 if using the applicative style for character generator
-- import Control.Applicative (liftA6)
-- Import replicateM if using it instead of vectorOf
-- import Control.Monad (replicateM)


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

-- | Calculates the ability modifier.
-- Subtract 10 from the ability score, divide by 2, and round down.
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- | Generates a random ability score.
-- Rolls four 6-sided dice, drops the lowest roll, and sums the remaining three.
ability :: Gen Int
ability = sum . drop 1 . sort <$> vectorOf 4 (choose (1, 6 :: Int))

-- | Generates a random character.
-- Generates six ability scores using the `ability` generator,
-- calculates hitpoints based on constitution, and constructs the Character.
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return Character { strength     = str
                   , dexterity    = dex
                   , constitution = con
                   , intelligence = int
                   , wisdom       = wis
                   , charisma     = cha
                   , hitpoints    = hp
                   }

-- Alternative applicative style implementation for character:
-- character :: Gen Character
-- character = liftA6 buildCharacter ability ability ability ability ability ability
--   where
--     buildCharacter str dex con int wis cha =
--       Character { strength     = str
--                 , dexterity    = dex
--                 , constitution = con
--                 , intelligence = int
--                 , wisdom       = wis
--                 , charisma     = cha
--                 , hitpoints    = 10 + modifier con
--                 }
