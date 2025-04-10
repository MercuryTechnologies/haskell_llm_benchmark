module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array)
  where
    binarySearch :: (Int, Int) -> Maybe Int
    binarySearch (low, high)
      | low > high = Nothing
      | otherwise  = let mid = (low + high) `div` 2
                         midValue = array ! mid
                     in if x == midValue
                        then Just mid
                        else if x < midValue
                             then binarySearch (low, mid - 1)
                             else binarySearch (mid + 1, high)
