module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Beverage = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

data House = House { resident :: Resident
                   , color :: Color
                   , pet :: Pet
                   , beverage :: Beverage
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

isValid :: [House] -> Bool
isValid houses = and [ englishmanRed
                     , spaniardDog
                     , ukrainianTea
                     , greenCoffee
                     , greenRightOfIvory
                     , oldGoldSnails
                     , koolsYellow
                     , milkMiddle
                     , norwegianFirst
                     , chesterfieldsNextToFox
                     , koolsNextToHorse
                     , luckyStrikeOrangeJuice
                     , japaneseParliaments
                     , norwegianNextToBlue
                     ]
  where
    englishmanRed = englishmanLivesInRedHouse houses
    spaniardDog = spaniardOwnsDog houses
    ukrainianTea = ukrainianDrinksTea houses
    greenCoffee = coffeeDrunkInGreenHouse houses
    greenRightOfIvory = greenHouseIsRightOfIvoryHouse houses
    oldGoldSnails = oldGoldSmokerOwnsSnails houses
    koolsYellow = koolsSmokedInYellowHouse houses
    milkMiddle = milkDrunkInMiddleHouse houses
    norwegianFirst = norwegianLivesInFirstHouse houses
    chesterfieldsNextToFox = chesterfieldsSmokerLivesNextToManWithFox houses
    koolsNextToHorse = koolsSmokedNextToHouseWithHorse houses
    luckyStrikeOrangeJuice = luckyStrikeSmokerDrinksOrangeJuice houses
    japaneseParliaments = japaneseSmokesParliaments houses
    norwegianNextToBlue = norwegianLivesNextToBlueHouse houses

-- Helper functions to check the given statements
englishmanLivesInRedHouse :: [House] -> Bool
englishmanLivesInRedHouse houses = elem (House Englishman Red _ _ _) houses

spaniardOwnsDog :: [House] -> Bool
spaniardOwnsDog houses = elem (House Spaniard _ Dog _ _) houses

ukrainianDrinksTea :: [House] -> Bool
ukrainianDrinksTea houses = elem (House Ukrainian _ _ Tea _) houses

coffeeDrunkInGreenHouse :: [House] -> Bool
coffeeDrunkInGreenHouse houses = elem (House _ Green _ Coffee _) houses

greenHouseIsRightOfIvoryHouse :: [House] -> Bool
greenHouseIsRightOfIvoryHouse houses = case [ (i, c) | (i, House _ c _ _ _) <- zip [1..] houses, c `elem` [Green, Ivory]] of
  [(a, Ivory), (b, Green)] -> b == a + 1
  _ -> False

oldGoldSmokerOwnsSnails :: [House] -> Bool
oldGoldSmokerOwnsSnails houses = elem (House _ _ Snails _ OldGold) houses

koolsSmokedInYellowHouse :: [House] -> Bool
koolsSmokedInYellowHouse houses = elem (House _ Yellow _ _ Kools) houses

milkDrunkInMiddleHouse :: [House] -> Bool
milkDrunkInMiddleHouse houses = beverage (houses !! 2) == Milk

norwegianLivesInFirstHouse :: [House] -> Bool
norwegianLivesInFirstHouse houses = resident (head houses) == Norwegian

chesterfieldsSmokerLivesNextToManWithFox :: [House] -> Bool
chesterfieldsSmokerLivesNextToManWithFox houses = case [ (i, p) | (i, House _ _ p _ _) <- zip [1..] houses, p == Fox] of
  [(a, _)] -> elem (House _ _ _ _ Chesterfields) (take 2 (drop (a-2) houses))
  _ -> False

koolsSmokedNextToHouseWithHorse :: [House] -> Bool
koolsSmokedNextToHouseWithHorse houses = case [ (i, p) | (i, House _ _ p _ _) <- zip [1..] houses, p == Horse] of
  [(a, _)] -> elem (House _ _ _ _ Kools) (take 2 (drop (a-2) houses))
  _ -> False

luckyStrikeSmokerDrinksOrangeJuice :: [House] -> Bool
luckyStrikeSmokerDrinksOrangeJuice houses = elem (House _ _ _ OrangeJuice LuckyStrike) houses

japaneseSmokesParliaments :: [House] -> Bool
japaneseSmokesParliaments houses = elem (House Japanese _ _ _ Parliaments) houses

norwegianLivesNextToBlueHouse :: [House] -> Bool
norwegianLivesNextToBlueHouse houses = case [ (i, c) | (i, House _ c _ _ _) <- zip [1..] houses, c == Blue] of
  [(a, _)] -> a == 2

solve :: Solution
solve = head [ Solution { waterDrinker = resident h, zebraOwner = resident z }
             | hs <- permutations [ House r c p b ci | r <- [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
                                                    , c <- [Red, Green, Ivory, Yellow, Blue]
                                                    , p <- [Dog, Snails, Fox, Horse, Zebra]
                                                    , b <- [Coffee, Tea, Milk, OrangeJuice, Water]
                                                    , ci <- [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
                                                    ]
             , let houses = take 5 hs
             , isValid houses
             , let h = head [ h | h <- houses, beverage h == Water]
             , let z = head [ h | h <- houses, pet h == Zebra]
             ]
