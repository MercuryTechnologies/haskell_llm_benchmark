module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = knapsack capacity items (length items)
  where
    knapsack _ [] _ = 0
    knapsack 0 _ _ = 0
    knapsack c ((w, v):xs) n
      | w > c = knapsack c xs (n - 1)
      | otherwise = max (knapsack c xs (n - 1)) (v + knapsack (c - w) xs (n - 1))
