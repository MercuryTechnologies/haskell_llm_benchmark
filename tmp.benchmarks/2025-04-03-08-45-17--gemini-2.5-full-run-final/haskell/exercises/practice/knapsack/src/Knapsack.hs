module Knapsack (maximumValue) where

-- | Solves the 0/1 Knapsack problem.
-- Given a maximum capacity and a list of items represented as (weight, value) tuples,
-- calculates the maximum total value of items that can be carried without exceeding the capacity.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = solve capacity items
  where
    -- solve :: remainingCapacity -> remainingItems -> maxValue
    solve :: Int -> [(Int, Int)] -> Int
    -- Base case: No more items left, so no more value can be added.
    solve _ [] = 0
    -- Recursive step: Consider the next item.
    solve currentCapacity ((itemWeight, itemValue) : remainingItems) =
      -- Option 1: Calculate the maximum value without taking the current item.
      let valueWithoutItem = solve currentCapacity remainingItems
          -- Option 2: Calculate the maximum value if we take the current item.
          -- This is only possible if the item's weight is within the current capacity.
          valueWithItem = if itemWeight <= currentCapacity
                            -- If taken, add its value and solve for the remaining capacity and items.
                            then itemValue + solve (currentCapacity - itemWeight) remainingItems
                            -- If it cannot be taken (too heavy), this path yields 0 additional value from this item onwards in this specific recursive branch.
                            -- Note: We still need to compare with valueWithoutItem. A better way might be to just not consider this branch,
                            -- but setting it to 0 works for the max comparison. Let's refine this slightly for clarity.
                            else 0 -- Cannot take this item, so the value contribution *from taking it* is 0.

      -- If the item fits, choose the better option (take it or leave it).
      in if itemWeight <= currentCapacity
           then max valueWithoutItem valueWithItem
           -- If the item doesn't fit, we *must* skip it.
           else valueWithoutItem
