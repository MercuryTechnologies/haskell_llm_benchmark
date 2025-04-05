module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = 
    [ (a, b, c) 
    | a <- [1..(sum `div` 3)]
    , b <- [(a+1)..(sum `div` 2)]
    , let c = sum - a - b
    , a < b
    , b < c
    , a * a + b * b == c * c
    ]
