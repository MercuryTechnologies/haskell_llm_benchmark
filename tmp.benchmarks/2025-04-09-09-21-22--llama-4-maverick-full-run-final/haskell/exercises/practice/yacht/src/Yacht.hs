module Yacht (yacht, Category(..)) where

import Data.List (sort, nub)

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
  Ones         -> countDice 1 dice
  Twos         -> countDice 2 dice * 2
  Threes       -> countDice 3 dice * 3
  Fours        -> countDice 4 dice * 4
  Fives        -> countDice 5 dice * 5
  Sixes        -> countDice 6 dice * 6
  FullHouse    -> scoreFullHouse dice
  FourOfAKind  -> scoreFourOfAKind dice
  LittleStraight -> scoreStraight [1, 2, 3, 4, 5] dice
  BigStraight  -> scoreStraight [2, 3, 4, 5, 6] dice
  Choice       -> sum dice
  Yacht        -> scoreYacht dice

countDice :: Int -> [Int] -> Int
countDice target = length . filter (== target)

scoreFullHouse :: [Int] -> Int
scoreFullHouse dice = let counts = countOccurrences dice
                          hasThree = any (== 3) counts
                          hasTwo   = any (== 2) counts
                      in if hasThree && hasTwo then sum dice else 0

countOccurrences :: [Int] -> [Int]
countOccurrences dice = map (`countDice` dice) [1..6]

scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind dice = let counts = countOccurrences dice
                            maxCount = maximum counts
                            target = head $ filter (\x -> countDice x dice == maxCount) [1..6]
                        in if maxCount >= 4 then 4 * target else 0

scoreStraight :: [Int] -> [Int] -> Int
scoreStraight straight dice = if sort dice == straight then 30 else 0

scoreYacht :: [Int] -> Int
scoreYacht dice = if length (nub dice) == 1 then 50 else 0
