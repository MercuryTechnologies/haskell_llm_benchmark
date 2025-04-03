module Knapsack (maximumValue) where

import Data.Array

-- | Solves the 0/1 Knapsack problem.
-- Given a maximum capacity and a list of items represented as (weight, value) tuples,
-- calculates the maximum total value of items that can be carried without exceeding the capacity.
-- Each item can only be taken once.
maximumValue :: Int             -- ^ Maximum weight capacity of the knapsack
             -> [(Int, Int)]    -- ^ List of items as (weight, value) tuples
             -> Int             -- ^ Maximum achievable value
maximumValue maxW items
  -- Handle edge cases: negative capacity or no items.
  | maxW < 0 = 0
  | null items = 0
  | otherwise = memo ! (n, maxW) -- The result is in the bottom-right corner of the DP table
  where
    n = length items
    -- Create a 1-based indexed array of items for easier DP state mapping.
    -- itemArr ! i corresponds to the i-th item in the original list.
    itemArr = listArray (1, n) items

    -- Define the bounds for the DP table: (item index, weight capacity)
    -- Item indices range from 0 (no items) to n (all items).
    -- Weight capacities range from 0 to maxW.
    bounds = ((0, 0), (n, maxW))

    -- Create the DP table `memo` using Data.Array.
    -- memo ! (i, w) represents the maximum value using the first 'i' items with capacity 'w'.
    memo = array bounds $
           -- Base case: 0 items. Value is 0 for any capacity.
           [((0, w), 0) | w <- range (0, maxW)] ++
           -- Base case: 0 capacity. Value is 0 for any number of items.
           [((i, 0), 0) | i <- range (1, n)] ++
           -- Calculate values for items 1 to n and capacities 1 to maxW using the recurrence.
           [((i, w), calculateValue i w) | i <- range (1, n), w <- range (1, maxW)]

    -- Helper function to calculate the value for state (i, w) based on previous states.
    calculateValue i w =
      let -- Get the weight and value of the current item (the i-th item).
          (itemWeight, itemValue) = itemArr ! i

          -- Option 1: Don't take item 'i'.
          -- The value is the same as the maximum value achievable with the first 'i-1' items and capacity 'w'.
          valWithout = memo ! (i - 1, w)

          -- Option 2: Take item 'i' (only possible if its weight does not exceed the current capacity 'w').
          valWith
            -- If the item fits (itemWeight <= w):
            | itemWeight <= w =
                -- The value is the item's value plus the maximum value achievable with the first 'i-1' items
                -- and the remaining capacity (w - itemWeight).
                itemValue + memo ! (i - 1, w - itemWeight)
            -- If the item doesn't fit:
            | otherwise =
                -- Taking the item is not possible, so this option yields a value of 0 in the context of the max comparison.
                -- Since item values are positive, valWithout will be chosen correctly by max.
                0

      -- The maximum value for state (i, w) is the maximum of the two options: taking or not taking item 'i'.
      in max valWithout valWith
