module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (find, findIndex, permutations)
import Control.Monad (guard) -- Used for filtering in list comprehensions

-- Enumerations for each category
data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

-- Structure representing a single house
data House = House
  { position :: Int
  , nationality :: Resident
  , color :: Color
  , pet :: Pet
  , drink :: Drink
  , cigarette :: Cigarette
  } deriving (Show)

-- Type alias for the list of five houses
type Street = [House]

-- Solution structure (provided)
data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper: Find the first house in the street satisfying a predicate
findHouse :: (House -> Bool) -> Street -> Maybe House
findHouse p = find p

-- Helper: Check if two types of houses (defined by predicates) are adjacent
isNextTo :: (House -> Bool) -> (House -> Bool) -> Street -> Bool
isNextTo p1 p2 street =
  case (findIndex p1 street, findIndex p2 street) of
    (Just i1, Just i2) -> abs (i1 - i2) == 1
    _                  -> False -- One or both houses not found

-- Main solver function
solve :: Solution
solve = head solutions -- The puzzle implies a unique solution
  where
    -- Generate lists of all possible values for each category
    nationals = [minBound..maxBound :: Resident]
    colors    = [minBound..maxBound :: Color]
    pets      = [minBound..maxBound :: Pet]
    drinks    = [minBound..maxBound :: Drink]
    cigs      = [minBound..maxBound :: Cigarette]
    positions = [1..5] -- House positions

    -- Generate all unique permutations for each category across the 5 houses
    allNatPerms = permutations nationals
    allColPerms = permutations colors
    allPetPerms = permutations pets
    allDrkPerms = permutations drinks
    allCigPerms = permutations cigs

    -- Pre-filter permutations based on fixed position and simple relative constraints
    -- Constraint 10: The Norwegian lives in the first house (index 0).
    c10Filter p = head p == Norwegian
    -- Constraint 9: Milk is drunk in the middle house (index 2).
    c9Filter p = (p !! 2) == Milk
    -- Constraint 15 (derived): Norwegian (house 1) is next to Blue. => Blue is house 2 (index 1).
    c15Filter p = (p !! 1) == Blue
    -- Constraint 6: The green house is immediately to the right of the ivory house.
    c6Filter p = case (findIndex (== Green) p, findIndex (== Ivory) p) of
                   (Just gIdx, Just iIdx) -> gIdx == iIdx + 1
                   _                      -> False -- Should not happen with full permutations

    -- Apply pre-filters
    validNatPerms = filter c10Filter allNatPerms
    validColPerms = filter c6Filter $ filter c15Filter allColPerms
    validDrkPerms = filter c9Filter allDrkPerms
    -- Pet and Cigarette permutations have no simple pre-filters based on position alone

    -- Function to check all direct link constraints (attributes within the same house)
    -- Takes the potential assignments for a single house index.
    checkDirectLinks :: Resident -> Color -> Pet -> Drink -> Cigarette -> Bool
    checkDirectLinks nat col pet drk cig =
      all (\f -> f nat col pet drk cig) directLinkChecks
      where
        directLinkChecks =
          [ \(n, c, _, _, _) -> not (n == Englishman) || c == Red             -- C2
          , \(n, _, p, _, _) -> not (n == Spaniard) || p == Dog               -- C3
          , \(_, c, _, d, _) -> not (c == Green) || d == Coffee               -- C4
          , \(n, _, _, d, _) -> not (n == Ukrainian) || d == Tea               -- C5
          , \(_, _, p, _, g) -> not (g == OldGold) || p == Snails             -- C7
          , \(_, c, _, _, g) -> not (c == Yellow) || g == Kools               -- C8
          , \(_, _, _, d, g) -> not (g == LuckyStrike) || d == OrangeJuice    -- C13
          , \(n, _, _, _, g) -> not (n == Japanese) || g == Parliaments       -- C14
          ]

    -- Generate and filter potential street configurations using list comprehensions
    -- Apply constraints progressively to prune the search space early.
    possibleStreets :: [Street]
    possibleStreets =
      [ street -- Construct the street *after* passing guards

      -- Iterate through pre-filtered permutations
      | natP <- validNatPerms
      , colP <- validColPerms
      , petP <- allPetPerms -- No pre-filter for pets
      , drkP <- validDrkPerms
      , cigP <- allCigPerms -- No pre-filter for cigarettes

      -- Apply direct link constraints using guards *before* building the street
      -- This checks if the assignments at each position are valid together.
      , guard $ all (\idx -> checkDirectLinks (natP !! idx) (colP !! idx) (petP !! idx) (drkP !! idx) (cigP !! idx)) [0..4]

      -- Build the street configuration *only* for valid combinations so far
      , let street = buildStreet natP colP petP drkP cigP positions

      -- Apply remaining relative constraints (adjacency) on the built street
      -- Constraint 11: The man who smokes Chesterfields lives next to the man with the fox.
      , guard $ isNextTo (\h -> cigarette h == Chesterfields) (\h -> pet h == Fox) street
      -- Constraint 12: Kools are smoked in the house next to the house where the horse is kept.
      , guard $ isNextTo (\h -> cigarette h == Kools) (\h -> pet h == Horse) street
      -- Note: C15 was used as a pre-filter. C6 was used as a pre-filter.
      ]

    -- Helper function to construct a Street from a set of permutations
    buildStreet :: [Resident] -> [Color] -> [Pet] -> [Drink] -> [Cigarette] -> [Int] -> Street
    buildStreet natP colP petP drkP cigP poss =
      [ House pos (natP !! idx) (colP !! idx) (petP !! idx) (drkP !! idx) (cigP !! idx)
      | (pos, idx) <- zip poss [0..] -- Map 1-based position to 0-based list index
      ]

    -- Extract the final answer from the (assumed unique) valid street configuration
    solutions :: [Solution]
    solutions = do
      street <- possibleStreets -- Should now contain exactly one valid street, found efficiently
      -- Find the house where water is drunk
      waterHouse <- maybe [] pure (findHouse (\h -> drink h == Water) street)
      -- Find the house where the zebra is owned
      zebraHouse <- maybe [] pure (findHouse (\h -> pet h == Zebra) street)
      -- Construct the Solution object using the nationalities found
      return $ Solution { waterDrinker = nationality waterHouse
                        , zebraOwner   = nationality zebraHouse
                        }
