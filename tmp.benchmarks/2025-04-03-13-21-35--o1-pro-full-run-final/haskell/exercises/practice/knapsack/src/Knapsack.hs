module Knapsack (maximumValue) where

-- We implement a classic 0/1 Knapsack dynamic programming approach.
-- maximumValue capacity items uses a 2D table where
-- table!!i!!w is the maximum value achievable using the first i items
-- with a knapsack capacity of w.

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = table !! length items !! capacity
  where
    -- table is built row by row:
    --  - The row index i goes from 0 (no items) to length items
    --  - The column index w goes from 0..capacity
    table = [[ bestVal i w | w <- [0..capacity] ]
                          | i <- [0..length items] ]

    bestVal :: Int -> Int -> Int
    bestVal 0 _ = 0
    bestVal i w =
      let (itemWeight, itemValue) = items !! (i - 1)
      in if itemWeight > w
         then table !! (i - 1) !! w
         else max (table !! (i - 1) !! w)
                  (itemValue + table !! (i - 1) !! (w - itemWeight))
