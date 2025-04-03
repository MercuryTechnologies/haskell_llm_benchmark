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
yacht Ones dice = sum [1 | d <- dice, d == 1]
yacht Twos dice = sum [2 | d <- dice, d == 2]
yacht Threes dice = sum [3 | d <- dice, d == 3]
yacht Fours dice = sum [4 | d <- dice, d == 4]
yacht Fives dice = sum [5 | d <- dice, d == 5]
yacht Sixes dice = sum [6 | d <- dice, d == 6]
yacht FullHouse dice = 
  let grouped = group $ sort dice
  in if length grouped == 2 && (length (head grouped) == 2 || length (head grouped) == 3)
     then sum dice
     else 0
yacht FourOfAKind dice =
  let grouped = group $ sort dice
      fourOrMoreOfKind = filter (\g -> length g >= 4) grouped
  in if not (null fourOrMoreOfKind)
     then 
       let value = head $ head fourOrMoreOfKind
       in 4 * value
     else 0
yacht LittleStraight dice =
  if sort dice == [1, 2, 3, 4, 5]
  then 30
  else 0
yacht BigStraight dice =
  if sort dice == [2, 3, 4, 5, 6]
  then 30
  else 0
yacht Choice dice = sum dice
yacht Yacht dice =
  if all (== head dice) (tail dice)
  then 50
  else 0
