module Knapsack (maximumValue) where

import Data.Array
import Data.List (sortOn)

-- | Calculates the maximum value that can be obtained with a knapsack of capacity n
-- | and the given items (each with a weight and value).
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items
  | capacity <= 0 = 0
  | null items = 0
  | otherwise = table ! (numItems, capacity)
  where
    numItems = length items
    -- Create array of items for efficient lookup
    itemsArray = listArray (1, numItems) items
    
    -- Create dynamic programming table
    table :: Array (Int, Int) Int
    table = array ((0, 0), (numItems, capacity)) 
            [((i, w), solve i w) | i <- [0..numItems], w <- [0..capacity]]
    
    -- Dynamic programming solution
    solve 0 _ = 0
    solve i w = 
      let (itemWeight, itemValue) = itemsArray ! i
      in if itemWeight > w
         then table ! (i-1, w)  -- Can't include this item
         else max (table ! (i-1, w))  -- Don't take item
                  (itemValue + table ! (i-1, w - itemWeight))  -- Take item
