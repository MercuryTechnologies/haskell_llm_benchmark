module Yacht (yacht, Category(..)) where

import Data.List (sort, group)

data Category
    = Ones
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
yacht category dice =
  case category of
    Ones           -> scoreSingles 1 dice
    Twos           -> scoreSingles 2 dice
    Threes         -> scoreSingles 3 dice
    Fours          -> scoreSingles 4 dice
    Fives          -> scoreSingles 5 dice
    Sixes          -> scoreSingles 6 dice
    FullHouse      -> scoreFullHouse dice
    FourOfAKind    -> scoreFourOfAKind dice
    LittleStraight -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight    -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice         -> sum dice
    Yacht          -> if length (group (sort dice)) == 1 then 50 else 0
  where
    scoreSingles n ds = n * length (filter (== n) ds)

    scoreFullHouse ds =
      let counts = sort (map length (group (sort ds)))
      in if counts == [2,3] then sum ds else 0

    scoreFourOfAKind ds =
      case filter (\g -> length g >= 4) (group (sort ds)) of
        (g:_) -> head g * 4
        _     -> 0
