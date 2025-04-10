module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight items = table' !! numItems !! maxWeight
  where
    numItems = length items
    table = [[0 | _ <- [0..maxWeight]] | _ <- [0..numItems]]
    table' = foldl buildTable table (zip [1..] items)
    buildTable tab (i, (weight, value)) = 
      let prevRow = tab !! (i-1)
          newRow = [if j < weight then prevRow !! j else max (prevRow !! j) (prevRow !! (j-weight) + value) | j <- [0..maxWeight]]
      in tab `replaceAt` i $ newRow
    replaceAt xs i x = take i xs ++ [x] ++ drop (i+1) xs
