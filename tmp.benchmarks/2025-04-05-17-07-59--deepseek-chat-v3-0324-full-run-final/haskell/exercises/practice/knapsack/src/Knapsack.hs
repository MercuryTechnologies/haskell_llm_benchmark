module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight items = dp maxWeight (length items)
  where
    dp w 0 = 0
    dp w i
      | wi > w = dp w (i-1)
      | otherwise = max (dp w (i-1)) (vi + dp (w - wi) (i-1))
      where
        (wi, vi) = items !! (i-1)
