module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array) 
  where
    binarySearch (low, high)
      | high < low = Nothing
      | otherwise =
          let mid = low + (high - low) `div` 2
              midVal = array ! mid
          in case compare x midVal of
               EQ -> Just mid
               LT -> binarySearch (low, mid - 1)
               GT -> binarySearch (mid + 1, high)
