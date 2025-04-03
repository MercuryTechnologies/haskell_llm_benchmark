module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sumN =
    [ (a, b, c)
    | a <- [1..(sumN - 2)]
    , b <- [a + 1..(sumN - a - 1)]
    , let c = sumN - a - b
    , b < c
    , a^2 + b^2 == c^2
    ]
