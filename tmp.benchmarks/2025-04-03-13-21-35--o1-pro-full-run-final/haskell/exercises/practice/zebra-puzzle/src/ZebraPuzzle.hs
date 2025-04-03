module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)
import Control.Monad (guard)

data Resident
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving (Eq, Show)

data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  }
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data House = House
  { color    :: Color
  , occupant :: Resident
  , drink    :: Drink
  , smoke    :: Smoke
  , pet      :: Pet
  }
  deriving (Eq, Show)

-- | Return the occupant who drinks water and the occupant who owns the zebra.
-- We prune the search space by applying most constraints *before* generating the final arrangement.
solve :: Solution
solve =
  case arrangements of
    (valid : _) ->
      let waterDrinkerResident = occupant (head [h | h <- valid, drink h == Water])
          zebraOwnerResident   = occupant (head [h | h <- valid, pet   h == Zebra])
      in Solution waterDrinkerResident zebraOwnerResident
    [] -> error "No valid solution found."

-- All possible house arrangements that satisfy all puzzle constraints.
arrangements :: [[House]]
arrangements = do
  -- Residents. We know from clue (10) that the Norwegian lives in the first house (index 0).
  let allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
  occupantPerm <- permutations allResidents
  guard (head occupantPerm == Norwegian)

  -- Colors. We know from clue (6) that green is immediately to the right of ivory.
  let allColors = [Red, Green, Ivory, Yellow, Blue]
  colorPerm <- permutations allColors
  let idxIvory = indexOf Ivory colorPerm
      idxGreen = indexOf Green colorPerm
  guard (idxGreen == idxIvory + 1)

  -- Drinks. We know from clue (9) that milk is in the 3rd house (index 2).
  let allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]
  drinkPerm <- permutations allDrinks
  guard (drinkPerm !! 2 == Milk)

  -- We also know from clue (5) that the Ukrainian drinks tea.
  guard (indexOf Ukrainian occupantPerm == indexOf Tea drinkPerm)

  -- We know from clue (4) that coffee is drunk in the green house.
  guard (indexOf Coffee drinkPerm == indexOf Green colorPerm)

  -- Smokes. A full permutation of the 5 brands.
  let allSmokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  smokePerm <- permutations allSmokes

  -- Pets. A full permutation of the 5 pets.
  let allPets = [Dog, Snails, Fox, Horse, Zebra]
  petPerm <- permutations allPets

  -- Combine them into House records for final adjacency checks.
  let houses = zipWith5 House colorPerm occupantPerm drinkPerm smokePerm petPerm

  -- Apply direct constraints that do not involve adjacency:
  -- (2) The Englishman lives in the red house
  guard (indexOf Englishman occupantPerm == indexOf Red colorPerm)
  -- (3) The Spaniard owns the dog
  guard (indexOf Spaniard occupantPerm == indexOf Dog petPerm)
  -- (7) The Old Gold smoker owns snails
  guard (indexOf OldGold smokePerm == indexOf Snails petPerm)
  -- (8) Kools are smoked in the yellow house
  guard (indexOf Kools smokePerm == indexOf Yellow colorPerm)
  -- (13) The Lucky Strike smoker drinks orange juice
  guard (indexOf LuckyStrike smokePerm == indexOf OrangeJuice drinkPerm)
  -- (14) The Japanese smokes Parliaments
  guard (indexOf Japanese occupantPerm == indexOf Parliaments smokePerm)

  -- Adjacency checks (11, 12, 15).
  guard (finalCheck houses)
  return houses

-- | Adjacency-based constraints: houses for Chesterfields <-> fox, Kools <-> horse, Norwegian <-> blue.
finalCheck :: [House] -> Bool
finalCheck houses =
  let iSmoke s = head [i | i <- [0..4], smoke    (houses !! i) == s]
      iPet p   = head [i | i <- [0..4], pet      (houses !! i) == p]
      iOcc r   = head [i | i <- [0..4], occupant (houses !! i) == r]
      iCol c   = head [i | i <- [0..4], color    (houses !! i) == c]

      neighbor a b = abs (a - b) == 1

      -- (11) Chesterfields <-> Fox
      chesterfieldsNextFox = neighbor (iSmoke Chesterfields) (iPet Fox)
      -- (12) Kools <-> Horse
      koolsNextHorse       = neighbor (iSmoke Kools) (iPet Horse)
      -- (15) Norwegian <-> Blue
      norwegianNextBlue    = neighbor (iOcc Norwegian) (iCol Blue)
  in and [chesterfieldsNextFox, koolsNextHorse, norwegianNextBlue]

-- | Similar to elemIndex but guaranteed to find the element because each list is a permutation of all possibilities.
indexOf :: (Eq a) => a -> [a] -> Int
indexOf x xs =
  case filter ((== x) . snd) (zip [0..] xs) of
    [(i, _)] -> i
    _        -> error "Should never happen: not found or duplicated."

-- | A helper for zipping 5 lists into one list of their combined elements.
zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> [a] -> [b] -> [c] -> [d] -> [e]
         -> [f]
zipWith5 _ [] _ _ _ _ = []
zipWith5 _ _ [] _ _ _ = []
zipWith5 _ _ _ [] _ _ = []
zipWith5 _ _ _ _ [] _ = []
zipWith5 _ _ _ _ _ [] = []
zipWith5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) =
  f a b c d e : zipWith5 f as bs cs ds es
