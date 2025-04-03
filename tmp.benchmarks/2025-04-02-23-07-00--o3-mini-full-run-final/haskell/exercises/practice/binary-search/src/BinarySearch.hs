module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = binarySearch lo hi
  where
    (lo, hi) = bounds arr
    binarySearch l h
      | l > h     = Nothing
      | otherwise =
          let mid = (l + h) `div` 2
              midVal = arr ! mid
          in case compare x midVal of
               EQ -> Just mid
               LT -> binarySearch l (mid - 1)
               GT -> binarySearch (mid + 1) h
