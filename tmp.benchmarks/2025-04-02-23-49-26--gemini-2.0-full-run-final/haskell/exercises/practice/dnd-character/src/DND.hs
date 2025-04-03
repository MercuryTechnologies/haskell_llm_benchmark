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
modifier abilityScore =
  abilityScore `div` 2 - 5

ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6))
  let sortedRolls = sort rolls
  return $ sum (tail sortedRolls)

character :: Gen Character
character = do
  strength'     <- ability
  dexterity'    <- ability
  constitution' <- ability
  intelligence' <- ability
  wisdom'       <- ability
  charisma'     <- ability
  let hitpoints' = 10 + modifier constitution'
  return Character
    { strength     = strength'
    , dexterity    = dexterity'
    , constitution = constitution'
    , intelligence = intelligence'
    , wisdom       = wisdom'
    , charisma     = charisma'
    , hitpoints    = hitpoints'
    }
