module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = knapsack capacity items
  where
    knapsack 0 _ = 0
    knapsack _ [] = 0
    knapsack w ((weight, value):rest)
      | weight > w = knapsack w rest
      | otherwise = max (knapsack w rest) (value + knapsack (w - weight) rest)
