module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show)
data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Eq, Show)
data Beverage = Water | Tea | Milk | OrangeJuice | Coffee deriving (Eq, Show)
data Cigarette = Kools | Chesterfields | OldGold | Parliaments | LuckyStrike deriving (Eq, Show)

data House = House { color :: Color
                   , resident :: Resident
                   , pet :: Pet
                   , beverage :: Beverage
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

type Solution = [House]

solve :: Solution
solve = head $ filter isValidSolution allPossibleSolutions
  where
    allPossibleSolutions = [ [House color resident pet beverage cigarette | color <- colors, resident <- residents, pet <- pets, beverage <- beverages, cigarette <- cigarettes] | colors <- permutations [Red, Green, Ivory, Yellow, Blue], residents <- permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese], pets <- permutations [Dog, Snails, Fox, Horse, Zebra], beverages <- permutations [Water, Tea, Milk, OrangeJuice, Coffee], cigarettes <- permutations [Kools, Chesterfields, OldGold, Parliaments, LuckyStrike] ]
    
    isValidSolution :: Solution -> Bool
    isValidSolution houses =
      let
        englishmanHouse = findHouse Englishman houses
        spaniardHouse = findHouse Spaniard houses
        ukrainianHouse = findHouse Ukrainian houses
        norwegianHouse = findHouse Norwegian houses
        japaneseHouse = findHouse Japanese houses
        greenHouse = findHouse Green houses
        ivoryHouse = findHouse Ivory houses
        yellowHouse = findHouse Yellow houses
        redHouse = findHouse Red houses
        middleHouse = houses !! 2
      in
        (color englishmanHouse == Red) &&
        (pet spaniardHouse == Dog) &&
        (beverage greenHouse == Coffee) &&
        (beverage ukrainianHouse == Tea) &&
        (nextTo (color greenHouse) (color ivoryHouse)) &&
        (pet (findHouse OldGold houses) == Snails) &&
        (cigarette yellowHouse == Kools) &&
        (beverage middleHouse == Milk) &&
        (resident norwegianHouse == Norwegian) &&
        (nextTo (cigarette (findHouse Chesterfields houses)) (pet (findHouse Fox houses))) &&
        (nextTo (cigarette yellowHouse) (pet (findHouse Horse houses))) &&
        (beverage (findHouse LuckyStrike houses) == OrangeJuice) &&
        (cigarette japaneseHouse == Parliaments) &&
        (nextTo (resident norwegianHouse) (resident (findHouse Blue houses)))

    findHouse :: Resident -> [House] -> House
    findHouse x houses = head [house | house <- houses, resident house == x]

    nextTo :: Eq a => a -> a -> Bool
    nextTo a b = abs (index a - index b) == 1
      where
        index x = case x of
          Red -> 0
          Green -> 1
          Ivory -> 2
          Yellow -> 3
          Blue -> 4
          _ -> -1

    colors = [Red, Green, Ivory, Yellow, Blue]
    residents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
    pets = [Dog, Snails, Fox, Horse, Zebra]
    beverages = [Water, Tea, Milk, OrangeJuice, Coffee]
    cigarettes = [Kools, Chesterfields, OldGold, Parliaments, LuckyStrike]
