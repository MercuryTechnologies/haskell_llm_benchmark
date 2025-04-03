module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue n items = knapsack n items
  where
    knapsack 0 _ = 0
    knapsack _ [] = 0
    knapsack capacity ((weight, value):xs)
      | weight > capacity = knapsack capacity xs
      | otherwise = max (value + knapsack (capacity - weight) xs) (knapsack capacity xs)
