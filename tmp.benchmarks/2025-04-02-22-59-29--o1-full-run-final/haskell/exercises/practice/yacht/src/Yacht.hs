module Yacht (yacht, Category(..)) where

import Data.List (sort, findIndex)

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
    deriving (Show, Eq)

yacht :: Category -> [Int] -> Int
yacht category dice =
  let countsDice = [ length (filter (== n) dice) | n <- [1..6] ]
      singles n  = n * (countsDice !! (n - 1))
  in case category of
       Ones           -> singles 1
       Twos           -> singles 2
       Threes         -> singles 3
       Fours          -> singles 4
       Fives          -> singles 5
       Sixes          -> singles 6
       FullHouse      ->
         let distinctCounts = filter (/= 0) countsDice
         in if length distinctCounts == 2 && sort distinctCounts == [2, 3]
            then sum dice
            else 0
       FourOfAKind    ->
         case findIndex (>= 4) countsDice of
           Just i  -> 4 * (i + 1)
           Nothing -> 0
       LittleStraight ->
         if sort dice == [1, 2, 3, 4, 5]
            then 30
            else 0
       BigStraight    ->
         if sort dice == [2, 3, 4, 5, 6]
            then 30
            else 0
       Choice         -> sum dice
       Yacht          ->
         if maximum countsDice == 5
            then 50
            else 0
