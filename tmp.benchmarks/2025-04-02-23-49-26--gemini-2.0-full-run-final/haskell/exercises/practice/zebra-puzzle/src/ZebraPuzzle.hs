module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad (guard)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show, Enum, Bounded)
data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Eq, Show, Enum, Bounded)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show, Enum, Bounded)
data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Eq, Show, Enum, Bounded)

data House = House { resident :: Resident
                   , color :: Color
                   , pet :: Pet
                   , drink :: Drink
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper function to check if a list contains only unique elements
allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = notElem x xs && allUnique xs

solve :: Solution
solve =
  let
    possibleHouses = sequence [enumFrom minBound .. enumFrom maxBound]
    
    -- Generate all possible house assignments
    possibleSolutions = do
      englishman <- enumFrom minBound :: [Resident]
      spaniard <- enumFrom minBound :: [Resident]
      ukrainian <- enumFrom minBound :: [Resident]
      norwegian <- enumFrom minBound :: [Resident]
      japanese <- enumFrom minBound :: [Resident]
      
      red <- enumFrom minBound :: [Color]
      green <- enumFrom minBound :: [Color]
      ivory <- enumFrom minBound :: [Color]
      yellow <- enumFrom minBound :: [Color]
      blue <- enumFrom minBound :: [Color]
      
      dog <- enumFrom minBound :: [Pet]
      snails <- enumFrom minBound :: [Pet]
      fox <- enumFrom minBound :: [Pet]
      horse <- enumFrom minBound :: [Pet]
      zebra <- enumFrom minBound :: [Pet]
      
      coffee <- enumFrom minBound :: [Drink]
      tea <- enumFrom minBound :: [Drink]
      milk <- enumFrom minBound :: [Drink]
      orangeJuice <- enumFrom minBound :: [Drink]
      water <- enumFrom minBound :: [Drink]
      
      oldGold <- enumFrom minBound :: [Cigarette]
      kools <- enumFrom minBound :: [Cigarette]
      chesterfields <- enumFrom minBound :: [Cigarette]
      luckyStrike <- enumFrom minBound :: [Cigarette]
      parliaments <- enumFrom minBound :: [Cigarette]

      guard (allUnique [englishman, spaniard, ukrainian, norwegian, japanese])
      guard (allUnique [red, green, ivory, yellow, blue])
      guard (allUnique [dog, snails, fox, horse, zebra])
      guard (allUnique [coffee, tea, milk, orangeJuice, water])
      guard (allUnique [oldGold, kools, chesterfields, luckyStrike, parliaments])

      let houses = zip5With House [englishman, spaniard, ukrainian, norwegian, japanese] [red, green, ivory, yellow, blue] [dog, snails, fox, horse, zebra] [coffee, tea, milk, orangeJuice, water] [oldGold, kools, chesterfields, luckyStrike, parliaments]

      return houses
  in
    error "Incomplete solution" -- Replace with actual solution finding logic
    -- head $ filter isValidSolution possibleSolutions
    -- where
    --   isValidSolution houses =
    --     (englishmanLivesInRedHouse houses) &&
    --     (spaniardOwnsDog houses) &&
    --     (coffeeIsDrunkInGreenHouse houses) &&
    --     (ukrainianDrinksTea houses) &&
    --     (greenHouseIsToRightOfIvoryHouse houses) &&
    --     (oldGoldSmokerOwnsSnails houses) &&
    --     (koolsAreSmokedInYellowHouse houses) &&
    --     (milkIsDrunkInMiddleHouse houses) &&
    --     (norwegianLivesInFirstHouse houses) &&
    --     (chesterfieldSmokerLivesNextToFoxOwner houses) &&
    --     (koolsAreSmokedNextToHorseOwner houses) &&
    --     (luckyStrikeSmokerDrinksOrangeJuice houses) &&
    --     (japaneseSmokesParliaments houses) &&
    --     (norwegianLivesNextToBlueHouse houses)

-- Constraint functions (to be implemented)
englishmanLivesInRedHouse :: [House] -> Bool
englishmanLivesInRedHouse _ = False

spaniardOwnsDog :: [House] -> Bool
spaniardOwnsDog _ = False

coffeeIsDrunkInGreenHouse :: [House] -> Bool
coffeeIsDrunkInGreenHouse _ = False

ukrainianDrinksTea :: [House] -> Bool
ukrainianDrinksTea _ = False

greenHouseIsToRightOfIvoryHouse :: [House] -> Bool
greenHouseIsToRightOfIvoryHouse _ = False

oldGoldSmokerOwnsSnails :: [House] -> Bool
oldGoldSmokerOwnsSnails _ = False

koolsAreSmokedInYellowHouse :: [House] -> Bool
koolsAreSmokedNextToHorseOwner :: [House] -> Bool
koolsAreSmokedNextToHorseOwner _ = False

luckyStrikeSmokerDrinksOrangeJuice :: [House] -> Bool
luckyStrikeSmokerDrinksOrangeJuice _ = False

japaneseSmokesParliaments :: [House] -> Bool
japaneseSmokesParliaments _ = False

norwegianLivesNextToBlueHouse :: [House] -> Bool
norwegianLivesNextToBlueHouse _ = False

zip5With :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zip5With f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e : zip5With f as bs cs ds es
zip5With _ _ _ _ _ _ = []
