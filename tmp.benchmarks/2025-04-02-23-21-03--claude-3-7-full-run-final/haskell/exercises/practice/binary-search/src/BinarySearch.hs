module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x
  | bounds array == (0, -1) = Nothing  -- Empty array case
  | otherwise = binarySearch array x lo hi
  where
    (lo, hi) = bounds array

binarySearch :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
binarySearch array x lo hi
  | lo > hi = Nothing  -- Element not found
  | array ! mid == x = Just mid  -- Element found
  | array ! mid > x = binarySearch array x lo (mid - 1)  -- Search in left half
  | otherwise = binarySearch array x (mid + 1) hi  -- Search in right half
  where
    mid = lo + (hi - lo) `div` 2
