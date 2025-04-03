module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, find)
import Data.Maybe (fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Beverage = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

data House = House { color :: Color
                   , resident :: Resident
                   , beverage :: Beverage
                   , pet :: Pet
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = head [ Solution { waterDrinker = resident (fromJust houseWithWater)
                        , zebraOwner = resident (fromJust houseWithZebra) }
             | houses <- permutations [House c r b p s | c <- [Red, Green, Ivory, Yellow, Blue]
                                                       , r <- [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
                                                       , b <- [Coffee, Tea, Milk, OrangeJuice, Water]
                                                       , p <- [Dog, Snails, Fox, Horse, Zebra]
                                                       , s <- [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]]
             , let houseWithWater = find (\h -> beverage h == Water) houses
             , let houseWithZebra = find (\h -> pet h == Zebra) houses
             , allConstraints houses
             ]

allConstraints :: [House] -> Bool
allConstraints houses = and
  [ length houses == 5
  , any (\h -> resident h == Englishman && color h == Red) houses
  , any (\h -> resident h == Spaniard && pet h == Dog) houses
  , any (\h -> beverage h == Coffee && color h == Green) houses
  , any (\h -> resident h == Ukrainian && beverage h == Tea) houses
  , any (\(h1, h2) -> color h1 == Ivory && color h2 == Green) (zip houses (tail houses))
  , any (\h -> cigarette h == OldGold && pet h == Snails) houses
  , any (\h -> cigarette h == Kools && color h == Yellow) houses
  , beverage (houses !! 2) == Milk
  , resident (head houses) == Norwegian
  , any (\(h1, h2) -> cigarette h1 == Chesterfields && pet h2 == Fox) (adjacentPairs houses)
  , any (\(h1, h2) -> cigarette h1 == Kools && pet h2 == Horse) (adjacentPairs houses)
  , any (\h -> cigarette h == LuckyStrike && beverage h == OrangeJuice) houses
  , any (\h -> resident h == Japanese && cigarette h == Parliaments) houses
  , any (\(h1, h2) -> resident h1 == Norwegian && color h2 == Blue) (adjacentPairs houses)
  ]

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zip xs (tail xs)
