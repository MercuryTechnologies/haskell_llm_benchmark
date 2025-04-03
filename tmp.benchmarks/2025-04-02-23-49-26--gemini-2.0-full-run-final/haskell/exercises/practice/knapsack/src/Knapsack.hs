module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = solve capacity items

solve :: Int -> [(Int, Int)] -> Int
solve capacity items =
  let n = length items
      dp = create2DArray (n + 1) (capacity + 1) 0
      create2DArray rows cols initialValue =
        [[initialValue | _ <- [1..cols]] | _ <- [1..rows]]
      
      -- Helper function to fill the DP table
      fillDP :: [[Int]] -> Int -> Int -> [[Int]]
      fillDP currentDP i w
        | i > n = currentDP
        | otherwise =
          let (weight, value) = items !! (i - 1)
              nextDP = 
                if weight > w
                then updateDP currentDP i w (currentDP !! (i-1) !! w)
                else updateDP currentDP i w (max (currentDP !! (i-1) !! w) (currentDP !! (i-1) !! (w - weight) + value))
          in fillDP nextDP (i + 1) w

      -- Helper function to update the DP table
      updateDP :: [[Int]] -> Int -> Int -> Int -> [[Int]]
      updateDP currentDP i w newValue =
        let updatedRow = take w (currentDP !! i) ++ [newValue] ++ drop (w + 1) (currentDP !! i)
        in take i currentDP ++ [updatedRow] ++ drop (i + 1) currentDP

      -- Initialize the DP table
      initialDP = create2DArray (n + 1) (capacity + 1) 0
      
      -- Fill the DP table
      finalDP = foldl (\acc w -> fillDP acc 1 w) initialDP [0..capacity]

  in last (head (reverse finalDP))
