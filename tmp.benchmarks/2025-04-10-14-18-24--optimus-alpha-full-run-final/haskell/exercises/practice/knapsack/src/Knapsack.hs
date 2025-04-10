module Knapsack (maximumValue) where

-- | maximumValue n items
--   n: maximum weight the knapsack can carry
--   items: list of (weight, value) pairs
--   returns: the maximum value that can be carried
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = knapsack capacity (length items)
  where
    -- Use memoization via a table for efficiency
    -- dp[i][w] = max value using first i items and capacity w
    knapsackTable = [[ bestValue i w | w <- [0..capacity] ] | i <- [0..length items] ]
      where
        bestValue 0 _ = 0
        bestValue _ 0 = 0
        bestValue i w =
          let (wi, vi) = items !! (i-1)
          in if wi > w
             then knapsackTable !! (i-1) !! w
             else max (knapsackTable !! (i-1) !! w)
                      (vi + knapsackTable !! (i-1) !! (w-wi))
    knapsack w i = knapsackTable !! i !! w
