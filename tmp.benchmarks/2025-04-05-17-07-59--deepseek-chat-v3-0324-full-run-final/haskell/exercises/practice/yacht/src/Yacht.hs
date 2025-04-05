module Yacht (yacht, Category(..)) where

import Data.List (sort)

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

count :: Int -> [Int] -> Int
count x = length . filter (== x)

sumDice :: [Int] -> Int
sumDice = sum

isYacht :: [Int] -> Bool
isYacht dice = all (== head dice) dice

isFourOfAKind :: [Int] -> Bool
isFourOfAKind dice = any (>=4) (map (`count` dice) dice)

isFullHouse :: [Int] -> Bool
isFullHouse dice = 
    let counts = map (`count` dice) [1..6]
    in elem 3 counts && elem 2 counts

isLittleStraight :: [Int] -> Bool
isLittleStraight dice = sort dice == [1,2,3,4,5]

isBigStraight :: [Int] -> Bool
isBigStraight dice = sort dice == [2,3,4,5,6]

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
    Ones    -> count 1 dice * 1
    Twos    -> count 2 dice * 2
    Threes  -> count 3 dice * 3
    Fours   -> count 4 dice * 4
    Fives   -> count 5 dice * 5
    Sixes   -> count 6 dice * 6
    FullHouse -> if isFullHouse dice then sumDice dice else 0
    FourOfAKind -> 
        if isFourOfAKind dice 
        then 4 * head [x | x <- dice, count x dice >= 4] 
        else 0
    LittleStraight -> if isLittleStraight dice then 30 else 0
    BigStraight -> if isBigStraight dice then 30 else 0
    Choice -> sumDice dice
    Yacht -> if isYacht dice then 50 else 0
