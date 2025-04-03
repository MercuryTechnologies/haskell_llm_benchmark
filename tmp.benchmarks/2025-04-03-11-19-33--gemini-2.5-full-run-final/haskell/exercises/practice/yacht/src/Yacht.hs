module Yacht (yacht, Category(..)) where

import Data.List (sort, group)
import Data.Maybe (listToMaybe)

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

-- Helper function to count occurrences of each die value
-- Returns a list of (value, count) pairs, sorted by value.
-- Example: counts [1, 2, 1, 5, 1] == [(1, 3), (2, 1), (5, 1)]
counts :: [Int] -> [(Int, Int)]
counts dice = map (\g -> (head g, length g)) . group . sort $ dice

-- Helper function for scoring number categories (Ones to Sixes)
scoreNumber :: Int -> [Int] -> Int
scoreNumber n dice = n * length (filter (== n) dice)

yacht :: Category -> [Int] -> Int
yacht category dice =
  let sortedDice = sort dice
      diceCounts = counts dice -- e.g., [(1, 2), (3, 3)] for a full house
      totalSum = sum dice
  in case category of
    Ones           -> scoreNumber 1 dice
    Twos           -> scoreNumber 2 dice
    Threes         -> scoreNumber 3 dice
    Fours          -> scoreNumber 4 dice
    Fives          -> scoreNumber 5 dice
    Sixes          -> scoreNumber 6 dice

    FullHouse      ->
      -- Check if the counts are exactly [2, 3] or [3, 2] when sorted
      let countsList = map snd diceCounts -- Get just the counts, e.g., [2, 3]
      in if sort countsList == [2, 3] then totalSum else 0

    FourOfAKind    ->
      -- Find if any count is >= 4
      -- `fourOrMore` will be like `[(value, count)]` or `[]`
      let fourOrMore = filter ((>= 4) . snd) diceCounts
      in case listToMaybe fourOrMore of -- Get the first (and only possible) group
           Just (value, _) -> 4 * value -- Score is 4 times the value of the four dice
           Nothing         -> 0

    LittleStraight -> if sortedDice == [1, 2, 3, 4, 5] then 30 else 0
    BigStraight    -> if sortedDice == [2, 3, 4, 5, 6] then 30 else 0
    Choice         -> totalSum

    Yacht          ->
      -- Check if there's only one group after sorting (all dice are the same)
      -- i.e., the list of counts has only one element, e.g., [(4, 5)]
      if length diceCounts == 1 then 50 else 0
