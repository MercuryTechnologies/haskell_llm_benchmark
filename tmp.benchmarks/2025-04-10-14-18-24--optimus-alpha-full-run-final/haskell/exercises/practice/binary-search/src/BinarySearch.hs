module BinarySearch (find) where

import Data.Array

-- | Perform a binary search on a sorted array.
-- Returns the index of the element if found, otherwise Nothing.
find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = binarySearch (bounds arr)
  where
    binarySearch (low, high)
      | low > high = Nothing
      | otherwise =
          let mid = (low + high) `div` 2
              midVal = arr ! mid
          in case compare x midVal of
               LT -> binarySearch (low, mid - 1)
               GT -> binarySearch (mid + 1, high)
               EQ -> Just mid
