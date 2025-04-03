module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue _ [] = 0
maximumValue capacity ((w, v) : xs)
    | w > capacity = maximumValue capacity xs
    | otherwise    = max (maximumValue capacity xs)
                         (v + maximumValue (capacity - w) xs)
