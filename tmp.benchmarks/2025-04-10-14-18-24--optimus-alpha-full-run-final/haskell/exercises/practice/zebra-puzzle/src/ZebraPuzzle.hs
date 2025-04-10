module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)
import Data.Maybe (fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded, Ord)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded, Ord)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded, Ord)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded, Ord)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded, Ord)

data House = House
  { resident :: Resident
  , color    :: Color
  , pet      :: Pet
  , drink    :: Drink
  , smoke    :: Smoke
  } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper for zip5 (not in Prelude)
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e) : zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

-- Helper: all unique
allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

-- Helper: next to
nextTo :: Int -> Int -> Bool
nextTo a b = abs (a - b) == 1

-- Helper: right of (b is immediately right of a)
rightOf :: Int -> Int -> Bool
rightOf a b = b - a == 1

-- The main solver
solve :: Solution
solve = head solutions
  where
    -- Generate all possible orderings for each attribute
    colorOrders      = permutations [Red, Green, Ivory, Yellow, Blue]
    residentOrders   = permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
    petOrders        = permutations [Dog, Snails, Fox, Horse, Zebra]
    drinkOrders      = permutations [Coffee, Tea, Milk, OrangeJuice, Water]
    smokeOrders      = permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]

    -- Apply constraints as early as possible to prune the search space
    solutions =
      [ Solution { waterDrinker = residents !! waterIdx
                 , zebraOwner   = residents !! zebraIdx
                 }
      | residents <- filter (\rs -> head rs == Norwegian) residentOrders
      , colors    <- colorOrders
      , let blueIdx = elemIndex' Blue colors
            norwegianIdx = elemIndex' Norwegian residents
      , nextTo blueIdx norwegianIdx
      , let ivoryIdx = elemIndex' Ivory colors
            greenIdx = elemIndex' Green colors
      , rightOf ivoryIdx greenIdx
      , let englishmanIdx = elemIndex' Englishman residents
            redIdx = elemIndex' Red colors
      , englishmanIdx == redIdx
      , drinks <- filter (\ds -> ds !! 2 == Milk) drinkOrders
      , let ukrainianIdx = elemIndex' Ukrainian residents
            teaIdx = elemIndex' Tea drinks
      , ukrainianIdx == teaIdx
      , let coffeeIdx = elemIndex' Coffee drinks
            greenIdx' = elemIndex' Green colors
      , coffeeIdx == greenIdx'
      , smokes <- smokeOrders
      , let koolsIdx = elemIndex' Kools smokes
            yellowIdx = elemIndex' Yellow colors
      , koolsIdx == yellowIdx
      , let luckyIdx = elemIndex' LuckyStrike smokes
            ojIdx = elemIndex' OrangeJuice drinks
      , luckyIdx == ojIdx
      , let japaneseIdx = elemIndex' Japanese residents
            parliamentsIdx = elemIndex' Parliaments smokes
      , japaneseIdx == parliamentsIdx
      , pets <- petOrders
      , let spaniardIdx = elemIndex' Spaniard residents
            dogIdx = elemIndex' Dog pets
      , spaniardIdx == dogIdx
      , let oldGoldIdx = elemIndex' OldGold smokes
            snailsIdx = elemIndex' Snails pets
      , oldGoldIdx == snailsIdx
      , let chesterIdx = elemIndex' Chesterfields smokes
            foxIdx = elemIndex' Fox pets
      , nextTo chesterIdx foxIdx
      , let horseIdx = elemIndex' Horse pets
      , nextTo koolsIdx horseIdx
      , let waterIdx = elemIndex' Water drinks
            zebraIdx = elemIndex' Zebra pets
      ]

-- Helper: find index of element in list
elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x xs = fromJust $ lookup x (zip xs [0..])

