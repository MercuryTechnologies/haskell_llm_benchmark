module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, find)
import Data.Maybe (fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data House = House { resident :: Resident
                   , color :: Color
                   , pet :: Pet
                   , drink :: Drink
                   , smoke :: Smoke
                   } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = fromJust $ find isSolutionValid allPossibleSolutions
  where
    allPossibleSolutions = [ Solution { waterDrinker = residentWithDrink Water houses
                                      , zebraOwner = residentWithPet Zebra houses }
                           | residents <- permutations allResidents
                           , colors    <- permutations allColors
                           , pets      <- permutations allPets
                           , drinks    <- permutations allDrinks
                           , smokes    <- permutations allSmokes
                           , let houses = zipWith5 House residents colors pets drinks smokes
                           , constraints houses
                           ]

    allResidents = enumFrom (minBound :: Resident)
    allColors    = enumFrom (minBound :: Color)
    allPets      = enumFrom (minBound :: Pet)
    allDrinks    = enumFrom (minBound :: Drink)
    allSmokes    = enumFrom (minBound :: Smoke)

    residentWithDrink d hs = resident $ fromJust $ find (\h -> drink h == d) hs
    residentWithPet p hs = resident $ fromJust $ find (\h -> pet h == p) hs

isSolutionValid :: Solution -> Bool
isSolutionValid _ = True

constraints :: [House] -> Bool
constraints houses =
    length houses == 5 &&
    -- 2. The Englishman lives in the red house.
    has (\h -> resident h == Englishman && color h == Red) &&
    -- 3. The Spaniard owns the dog.
    has (\h -> resident h == Spaniard && pet h == Dog) &&
    -- 4. Coffee is drunk in the green house.
    has (\h -> drink h == Coffee && color h == Green) &&
    -- 5. The Ukrainian drinks tea.
    has (\h -> resident h == Ukrainian && drink h == Tea) &&
    -- 6. The green house is immediately to the right of the ivory house.
    rightOf (\l r -> color l == Ivory && color r == Green) &&
    -- 7. The Old Gold smoker owns snails.
    has (\h -> smoke h == OldGold && pet h == Snails) &&
    -- 8. Kools are smoked in the yellow house.
    has (\h -> smoke h == Kools && color h == Yellow) &&
    -- 9. Milk is drunk in the middle house.
    drink (houses !! 2) == Milk &&
    -- 10. The Norwegian lives in the first house.
    resident (head houses) == Norwegian &&
    -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    nextTo (\l r -> smoke l == Chesterfields && pet r == Fox) &&
    -- 12. Kools are smoked in the house next to the house where the horse is kept.
    nextTo (\l r -> smoke l == Kools && pet r == Horse) &&
    -- 13. The Lucky Strike smoker drinks orange juice.
    has (\h -> smoke h == LuckyStrike && drink h == OrangeJuice) &&
    -- 14. The Japanese smokes Parliaments.
    has (\h -> resident h == Japanese && smoke h == Parliaments) &&
    -- 15. The Norwegian lives next to the blue house.
    nextTo (\l r -> resident l == Norwegian && color r == Blue)
  where
    has p = any p houses
    rightOf p = any (uncurry p) (zip houses (tail houses))
    nextTo p = any (uncurry p) (zip houses (tail houses)) || any (uncurry p) (zip (tail houses) houses)

zipWith5 :: (a->b->c->d->e->f) -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) = z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _ = []

