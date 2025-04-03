module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n =
    [ (a, b, c)
    | a <- [1..n]
    , b <- [a+1..n]
    , let c = n - a - b
    , b < c
    , a^2 + b^2 == c^2
    ]
