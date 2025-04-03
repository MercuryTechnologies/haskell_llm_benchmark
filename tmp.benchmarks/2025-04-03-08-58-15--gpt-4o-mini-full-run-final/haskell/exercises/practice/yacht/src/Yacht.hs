module Yacht (yacht, Category(..)) where

import Data.List (sort, group)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
    Ones            -> scoreOf dice 1
    Twos            -> scoreOf dice 2
    Threes          -> scoreOf dice 3
    Fours           -> scoreOf dice 4
    Fives           -> scoreOf dice 5
    Sixes           -> scoreOf dice 6
    FullHouse       -> fullHouseScore dice
    FourOfAKind     -> fourOfAKindScore dice
    LittleStraight  -> if sortedDice == [1, 2, 3, 4, 5] then 30 else 0
    BigStraight     -> if sortedDice == [2, 3, 4, 5, 6] then 30 else 0
    Choice          -> sum dice
    Yacht           -> if allEqual dice then 50 else 0
  where
    scoreOf ds n = n * length (filter (== n) ds)
    fullHouseScore ds = if length (groupCounts ds) == 2 then sum ds else 0
    fourOfAKindScore ds = if any ((>= 4) . snd) (groupCounts ds) then sum (take 4 (filter (== fst (head (groupCounts ds))) ds)) else 0
    sortedDice = sort dice
    groupCounts xs = map (\x -> (head x, length x)) . group . sort $ xs
    allEqual xs = all (== head xs) xs
