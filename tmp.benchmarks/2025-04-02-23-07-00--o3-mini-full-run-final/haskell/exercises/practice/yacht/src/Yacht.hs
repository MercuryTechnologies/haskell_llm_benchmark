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
              deriving (Eq, Show)

yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones           -> scoreForNumber 1
    Twos           -> scoreForNumber 2
    Threes         -> scoreForNumber 3
    Fours          -> scoreForNumber 4
    Fives          -> scoreForNumber 5
    Sixes          -> scoreForNumber 6
    FullHouse      ->
      let groups = sort $ map length (group (sort dice))
      in if groups == [2,3] then sum dice else 0
    FourOfAKind    ->
      let groups = group (sort dice)
          four = filter (\g -> length g >= 4) groups
      in if null four then 0 else (head (head four)) * 4
    LittleStraight ->
      if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight    ->
      if sort dice == [2,3,4,5,6] then 30 else 0
    Choice         -> sum dice
    Yacht          ->
      if all (== head dice) dice then 50 else 0
  where
    scoreForNumber n = n * length (filter (== n) dice)
