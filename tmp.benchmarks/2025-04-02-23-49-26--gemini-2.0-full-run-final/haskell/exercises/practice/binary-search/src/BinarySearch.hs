module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binSearch array x (bounds array)

binSearch :: Ord a => Array Int a -> a -> (Int, Int) -> Maybe Int
binSearch array x (lower, upper)
  | lower > upper = Nothing
  | otherwise =
    let mid = lower + (upper - lower) `div` 2
        midValue = array ! mid
    in  case compare x midValue of
          EQ -> Just mid
          LT -> binSearch array x (lower, mid - 1)
          GT -> binSearch array x (mid + 1, upper)
