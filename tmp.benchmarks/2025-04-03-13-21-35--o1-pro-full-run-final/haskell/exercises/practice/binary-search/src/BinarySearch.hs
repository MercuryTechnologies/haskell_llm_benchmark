module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x =
    let
        (low, high) = bounds array

        go l h
          | l > h     = Nothing
          | otherwise =
              let mid = (l + h) `div` 2
                  val = array ! mid
              in if val == x
                 then Just mid
                 else if val < x
                      then go (mid + 1) h
                      else go l (mid - 1)
    in go low high
