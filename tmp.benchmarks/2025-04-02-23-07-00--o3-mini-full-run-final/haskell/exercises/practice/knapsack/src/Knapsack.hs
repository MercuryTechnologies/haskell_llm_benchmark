module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp !! capacity
  where
    dp = foldl update (replicate (capacity + 1) 0) items
    update currentDP (w, v) = [ if j < w then currentDP !! j
                                else max (currentDP !! j) ((currentDP !! (j - w)) + v)
                              | j <- [0..capacity] ]
