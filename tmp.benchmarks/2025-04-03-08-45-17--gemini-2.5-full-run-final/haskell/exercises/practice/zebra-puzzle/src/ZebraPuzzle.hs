module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (find, permutations)
import Data.Maybe (listToMaybe) -- Using listToMaybe instead of head for safety

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

-- Represent a single house with all its attributes
data House = House { houseNumber :: Int
                   , nationality :: Resident
                   , color :: Color
                   , pet :: Pet
                   , drink :: Drink
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

-- The complete puzzle solution is a list of 5 houses
type Puzzle = [House]

-- The required output format
data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper function to get all values of an Bounded Enum
allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound .. maxBound]

-- Helper to find a house matching a predicate
findHouse :: (House -> Bool) -> Puzzle -> Maybe House
findHouse = find -- find is already in Data.List

-- Helper for relative positions
isNextTo :: House -> House -> Bool
isNextTo h1 h2 = abs (houseNumber h1 - houseNumber h2) == 1

-- Helper for relative positions (h1 is immediately to the right of h2)
isRightOf :: House -> House -> Bool
isRightOf h1 h2 = houseNumber h1 == houseNumber h2 + 1

-- Predicates for each rule
-- Rule 2: The Englishman lives in the red house.
rule2 :: Puzzle -> Bool
rule2 p = any (\h -> nationality h == Englishman && color h == Red) p

-- Rule 3: The Spaniard owns the dog.
rule3 :: Puzzle -> Bool
rule3 p = any (\h -> nationality h == Spaniard && pet h == Dog) p

-- Rule 4: Coffee is drunk in the green house.
rule4 :: Puzzle -> Bool
rule4 p = any (\h -> drink h == Coffee && color h == Green) p

-- Rule 5: The Ukrainian drinks tea.
rule5 :: Puzzle -> Bool
rule5 p = any (\h -> nationality h == Ukrainian && drink h == Tea) p

-- Rule 6: The green house is immediately to the right of the ivory house.
rule6 :: Puzzle -> Bool
rule6 p = case (findHouse (\h -> color h == Green) p, findHouse (\h -> color h == Ivory) p) of
            (Just greenHouse, Just ivoryHouse) -> isRightOf greenHouse ivoryHouse
            _ -> False -- Should not happen in a valid permutation set

-- Rule 7: The Old Gold smoker owns snails.
rule7 :: Puzzle -> Bool
rule7 p = any (\h -> cigarette h == OldGold && pet h == Snails) p

-- Rule 8: Kools are smoked in the yellow house.
rule8 :: Puzzle -> Bool
rule8 p = any (\h -> cigarette h == Kools && color h == Yellow) p

-- Rule 9: Milk is drunk in the middle house (house 3).
rule9 :: Puzzle -> Bool
rule9 p = any (\h -> houseNumber h == 3 && drink h == Milk) p

-- Rule 10: The Norwegian lives in the first house (house 1).
rule10 :: Puzzle -> Bool
rule10 p = any (\h -> houseNumber h == 1 && nationality h == Norwegian) p

-- Rule 11: The man who smokes Chesterfields lives in the house next to the man with the fox.
rule11 :: Puzzle -> Bool
rule11 p = case (findHouse (\h -> cigarette h == Chesterfields) p, findHouse (\h -> pet h == Fox) p) of
             (Just chesterHouse, Just foxHouse) -> isNextTo chesterHouse foxHouse
             _ -> False

-- Rule 12: Kools are smoked in the house next to the house where the horse is kept.
rule12 :: Puzzle -> Bool
rule12 p = case (findHouse (\h -> cigarette h == Kools) p, findHouse (\h -> pet h == Horse) p) of
             (Just koolsHouse, Just horseHouse) -> isNextTo koolsHouse horseHouse
             _ -> False

-- Rule 13: The Lucky Strike smoker drinks orange juice.
rule13 :: Puzzle -> Bool
rule13 p = any (\h -> cigarette h == LuckyStrike && drink h == OrangeJuice) p

-- Rule 14: The Japanese smokes Parliaments.
rule14 :: Puzzle -> Bool
rule14 p = any (\h -> nationality h == Japanese && cigarette h == Parliaments) p

-- Rule 15: The Norwegian lives next to the blue house.
rule15 :: Puzzle -> Bool
rule15 p = case (findHouse (\h -> nationality h == Norwegian) p, findHouse (\h -> color h == Blue) p) of
             (Just norwegianHouse, Just blueHouse) -> isNextTo norwegianHouse blueHouse
             _ -> False

-- List of all rules to check
allRules :: [Puzzle -> Bool]
allRules = [ rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10
           , rule11, rule12, rule13, rule14, rule15
           ]

-- Check if a puzzle configuration satisfies all rules
isValid :: Puzzle -> Bool
isValid p = all (\rule -> rule p) allRules

-- The main solver function
solve :: Solution
solve =
  let
    -- Define all possible values for each category
    nationals   = allValues :: [Resident]
    colors      = allValues :: [Color]
    pets        = allValues :: [Pet]
    drinks      = allValues :: [Drink]
    cigarettes  = allValues :: [Cigarette]

    -- Generate permutations for each category
    -- Apply fixed position rules early to prune the search space
    permsNat = filter (\p -> p !! 0 == Norwegian) $ permutations nationals -- Rule 10
    permsDrk = filter (\p -> p !! 2 == Milk)      $ permutations drinks    -- Rule 9
    permsCol = permutations colors
    permsPet = permutations pets
    permsCig = permutations cigarettes

    -- Function to build a Puzzle from a combination of permutations
    -- zipWith combines the house number (index + 1) with the elements from each permutation list
    buildPuzzle :: [Resident] -> [Color] -> [Pet] -> [Drink] -> [Cigarette] -> Puzzle
    buildPuzzle ns cs ps ds cgs =
        zipWith (\i (((n, c), p), (d, cg)) -> House i n c p d cg) [1..5] $
        zip (zip (zip ns cs) ps) (zip ds cgs)


    -- Generate candidate puzzles by combining permutations
    -- Filter candidates using isValid as they are generated (or after)
    -- Using list comprehension for generation and filtering
    validSolutions =
      [ puzzle
      | nats <- permsNat
      , cols <- permsCol
      , pets <- permsPet
      , drks <- permsDrk
      , cigs <- permsCig
      , let puzzle = buildPuzzle nats cols pets drks cigs
      , isValid puzzle -- Check all rules here
      ]

    -- Extract the single solution (use listToMaybe for safety)
    theSolutionPuzzle = case listToMaybe validSolutions of
        Just p -> p
        Nothing -> error "No solution found!" -- Should not happen for this puzzle

    -- Find the water drinker and zebra owner from the solution puzzle
    findResident :: (House -> Bool) -> Resident
    findResident pred = case findHouse pred theSolutionPuzzle of
        Just h  -> nationality h
        Nothing -> error "Attribute not found in the final solution!" -- Indicates an error in logic or rules

    waterDrinkerNat = findResident (\h -> drink h == Water)
    zebraOwnerNat   = findResident (\h -> pet h == Zebra)

  in Solution { waterDrinker = waterDrinkerNat
              , zebraOwner   = zebraOwnerNat
              }
