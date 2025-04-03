module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x
  | isEmpty = Nothing
  | otherwise = binarySearch array x low high
  where
    (low, high) = bounds array
    isEmpty = low > high

binarySearch :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
binarySearch array x low high
  | low > high = Nothing
  | current == x = Just mid
  | current > x = binarySearch array x low (mid - 1)
  | otherwise = binarySearch array x (mid + 1) high
  where
    mid = low + (high - low) `div` 2
    current = array ! mid
