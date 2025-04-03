module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x =
  let (low, high) = bounds array
      go lo hi
        | lo > hi   = Nothing
        | otherwise =
            let mid   = (lo + hi) `div` 2
                midVal = array ! mid
            in case compare x midVal of
                 EQ -> Just mid
                 LT -> go lo (mid - 1)
                 GT -> go (mid + 1) hi
  in go low high
