module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)

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
  , color :: Color
  , resident :: Resident
  , pet :: Pet
  , drink :: Drink
  , cigarette :: Cigarette
  } deriving (Eq, Show)

type Houses = [House]

solve :: Solution
solve = Solution { waterDrinker = waterDrinker', zebraOwner = zebraOwner' }
  where
    houses = findSolution
    waterDrinker' = resident $ head $ filter ((== Water) . drink) houses
    zebraOwner' = resident $ head $ filter ((== Zebra) . pet) houses

findSolution :: Houses
findSolution = head $ filter isValid allPossibleHouses

isValid :: Houses -> Bool
isValid houses = 
  -- 2. The Englishman lives in the red house.
  residentInColorHouse Englishman Red houses &&
  -- 3. The Spaniard owns the dog.
  residentOwnsPet Spaniard Dog houses &&
  -- 4. Coffee is drunk in the green house.
  drinkInColorHouse Coffee Green houses &&
  -- 5. The Ukrainian drinks tea.
  residentDrinks Ukrainian Tea houses &&
  -- 6. The green house is immediately to the right of the ivory house.
  colorRightOfColor Green Ivory houses &&
  -- 7. The Old Gold smoker owns snails.
  cigaretteSmokerOwnsPet OldGold Snails houses &&
  -- 8. Kools are smoked in the yellow house.
  cigaretteInColorHouse Kools Yellow houses &&
  -- 9. Milk is drunk in the middle house.
  drinkInPosition Milk 3 houses &&
  -- 10. The Norwegian lives in the first house.
  residentInPosition Norwegian 1 houses &&
  -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
  nextToEachOther (\h -> cigarette h == Chesterfields) (\h -> pet h == Fox) houses &&
  -- 12. Kools are smoked in the house next to the house where the horse is kept.
  nextToEachOther (\h -> cigarette h == Kools) (\h -> pet h == Horse) houses &&
  -- 13. The Lucky Strike smoker drinks orange juice.
  cigaretteSmokerDrinks LuckyStrike OrangeJuice houses &&
  -- 14. The Japanese smokes Parliaments.
  residentSmokes Japanese Parliaments houses &&
  -- 15. The Norwegian lives next to the blue house.
  nextToEachOther (\h -> resident h == Norwegian) (\h -> color h == Blue) houses

-- Helper functions for checking constraints
residentInColorHouse :: Resident -> Color -> Houses -> Bool
residentInColorHouse r c houses = any (\h -> resident h == r && color h == c) houses

residentOwnsPet :: Resident -> Pet -> Houses -> Bool
residentOwnsPet r p houses = any (\h -> resident h == r && pet h == p) houses

drinkInColorHouse :: Drink -> Color -> Houses -> Bool
drinkInColorHouse d c houses = any (\h -> drink h == d && color h == c) houses

residentDrinks :: Resident -> Drink -> Houses -> Bool
residentDrinks r d houses = any (\h -> resident h == r && drink h == d) houses

colorRightOfColor :: Color -> Color -> Houses -> Bool
colorRightOfColor c1 c2 houses = 
  any (\h1 -> any (\h2 -> color h1 == c1 && color h2 == c2 && position h1 == position h2 + 1) houses) houses

cigaretteSmokerOwnsPet :: Cigarette -> Pet -> Houses -> Bool
cigaretteSmokerOwnsPet c p houses = any (\h -> cigarette h == c && pet h == p) houses

cigaretteInColorHouse :: Cigarette -> Color -> Houses -> Bool
cigaretteInColorHouse c col houses = any (\h -> cigarette h == c && color h == col) houses

drinkInPosition :: Drink -> Int -> Houses -> Bool
drinkInPosition d p houses = any (\h -> drink h == d && position h == p) houses

residentInPosition :: Resident -> Int -> Houses -> Bool
residentInPosition r p houses = any (\h -> resident h == r && position h == p) houses

nextToEachOther :: (House -> Bool) -> (House -> Bool) -> Houses -> Bool
nextToEachOther pred1 pred2 houses =
  any (\h1 -> any (\h2 -> pred1 h1 && pred2 h2 && abs (position h1 - position h2) == 1) houses) houses

cigaretteSmokerDrinks :: Cigarette -> Drink -> Houses -> Bool
cigaretteSmokerDrinks c d houses = any (\h -> cigarette h == c && drink h == d) houses

residentSmokes :: Resident -> Cigarette -> Houses -> Bool
residentSmokes r c houses = any (\h -> resident h == r && cigarette h == c) houses

-- Generate all possible house configurations
allPossibleHouses :: [Houses]
allPossibleHouses = do
  -- Generate all possible permutations of houses
  colorPerms <- permutations [Red, Green, Ivory, Yellow, Blue]
  residentPerms <- permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
  petPerms <- permutations [Dog, Snails, Fox, Horse, Zebra]
  drinkPerms <- permutations [Coffee, Tea, Milk, OrangeJuice, Water]
  cigarettePerms <- permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  
  -- Create houses with fixed positions 1-5
  let houses = zipWith5 (\col res pet drink cig pos -> 
                          House { position = pos
                                , color = col
                                , resident = res
                                , pet = pet
                                , drink = drink
                                , cigarette = cig
                                })
                        colorPerms residentPerms petPerms drinkPerms cigarettePerms [1..5]
  
  return houses

-- Helper function to get the position in the zipWith5
zipWith5 :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = f a b c d e f : zipWith5 f as bs cs ds es fs
zipWith5 _ _ _ _ _ _ _ = []
