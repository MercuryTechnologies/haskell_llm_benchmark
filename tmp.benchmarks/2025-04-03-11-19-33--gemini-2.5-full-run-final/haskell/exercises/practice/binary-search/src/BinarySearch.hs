module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = search low high
  where
    (low, high) = bounds array

    search l h
      | l > h = Nothing -- Base case: element not found
      | otherwise =
          let mid = l + (h - l) `div` 2 -- Calculate middle index
              midVal = array ! mid       -- Get value at middle index
          in case compare midVal x of
               EQ -> Just mid          -- Element found at mid
               LT -> search (mid + 1) h -- Search in the right half
               GT -> search l (mid - 1) -- Search in the left half
