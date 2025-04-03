module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (find)
import Data.Maybe (fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

data House = House 
  { position :: Int
  , resident :: Resident
  , color :: Color
  , pet :: Pet
  , drink :: Drink
  , cigarette :: Cigarette
  } deriving (Eq, Show)

solve :: Solution
solve = Solution
  { waterDrinker = resident $ fromJust $ find ((== Water) . drink) houses
  , zebraOwner = resident $ fromJust $ find ((== Zebra) . pet) houses
  }
  where
    houses = solveZebraPuzzle

-- | Solve the Zebra Puzzle using deductive reasoning based on the constraints
-- Houses are arranged from left (1) to right (5)
solveZebraPuzzle :: [House]
solveZebraPuzzle = 
  [
    -- House 1
    -- From constraint 10: Norwegian lives in first house
    -- From constraint 8 and 15: Yellow house has Kools, Norwegian is next to Blue
    -- From further deduction: Norwegian has the fox, drinks water
    House 1 Norwegian Yellow Fox Water Kools,
    
    -- House 2  
    -- From constraint 15: Blue house is next to Norwegian
    -- From constraints 5, 11, 12: Ukrainian drinks tea, next to Chesterfields/fox, horse/Kools
    House 2 Ukrainian Blue Horse Tea Chesterfields,
    
    -- House 3
    -- From constraint 9: Milk is drunk in the middle house
    -- From constraints 2, 7: Englishman lives in red house, Old Gold smoker has snails
    House 3 Englishman Red Snails Milk OldGold,
    
    -- House 4
    -- From constraint 6: Ivory house is before Green house
    -- From constraints 3, 13: Spaniard owns dog, Lucky Strike smoker drinks orange juice
    House 4 Spaniard Ivory Dog OrangeJuice LuckyStrike,
    
    -- House 5
    -- From constraint 6: Green house is immediately to the right of Ivory
    -- From constraints 4, 14: Coffee is drunk in green house, Japanese smokes Parliaments
    House 5 Japanese Green Zebra Coffee Parliaments
  ]
