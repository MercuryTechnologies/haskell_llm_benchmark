module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n =
  [ (a, b, c)
  | a <- [1 .. n `div` 3]
  , b <- [a + 1 .. (n - a) `div` 2]
  , let c = n - a - b
  , a^2 + b^2 == c^2
  ]
