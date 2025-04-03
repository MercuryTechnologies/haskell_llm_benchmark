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
    Ones            -> scoreSingles 1
    Twos            -> scoreSingles 2
    Threes          -> scoreSingles 3
    Fours           -> scoreSingles 4
    Fives           -> scoreSingles 5
    Sixes           -> scoreSingles 6
    FullHouse       -> scoreFullHouse
    FourOfAKind     -> scoreFourOfAKind
    LittleStraight  -> if sort dice == [1, 2, 3, 4, 5] then 30 else 0
    BigStraight     -> if sort dice == [2, 3, 4, 5, 6] then 30 else 0
    Choice          -> sum dice
    Yacht           -> if length (group dice) == 1 then 50 else 0
  where
    scoreSingles n = n * length (filter (== n) dice)
    scoreFullHouse = if length groups == 2 && (length (head groups) == 3 || length (head groups) == 2)
                     then sum dice
                     else 0
    scoreFourOfAKind = case filter ((>= 4) . length) groups of
                         (x:_) -> sum (take 4 x)
                         _     -> 0
    groups = group (sort dice)
