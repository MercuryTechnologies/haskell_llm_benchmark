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
yacht Ones dice = sum [d | d <- dice, d == 1]
yacht Twos dice = sum [d | d <- dice, d == 2]
yacht Threes dice = sum [d | d <- dice, d == 3]
yacht Fours dice = sum [d | d <- dice, d == 4]
yacht Fives dice = sum [d | d <- dice, d == 5]
yacht Sixes dice = sum [d | d <- dice, d == 6]

yacht FullHouse dice
  | isFullHouse dice = sum dice
  | otherwise = 0
  where
    isFullHouse ds = 
      let grouped = group $ sort ds
      in length grouped == 2 && (length (head grouped) == 2 || length (head grouped) == 3)

yacht FourOfAKind dice =
  let grouped = group $ sort dice
      fourOrMore = filter (\g -> length g >= 4) grouped
  in if null fourOrMore
     then 0
     else (head $ head fourOrMore) * 4

yacht LittleStraight dice
  | sort dice == [1, 2, 3, 4, 5] = 30
  | otherwise = 0

yacht BigStraight dice
  | sort dice == [2, 3, 4, 5, 6] = 30
  | otherwise = 0

yacht Choice dice = sum dice

yacht Yacht dice
  | all (== head dice) dice = 50
  | otherwise = 0
