module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch array 0 (length array - 1) x

binarySearch :: Ord a => Array Int a -> Int -> Int -> a -> Maybe Int
binarySearch array low high x
    | low > high = Nothing
    | midValue == x = Just mid
    | midValue < x = binarySearch array (mid + 1) high x
    | otherwise = binarySearch array low (mid - 1) x
  where
    mid = (low + high) `div` 2
    midValue = array ! mid
