module Knapsack (maximumValue) where

-- | Calculate the maximum value that can be obtained with the given weight limit and items
-- Items are represented as (weight, value) pairs
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = last $ foldl buildRow initialRow items
  where
    initialRow = replicate (capacity + 1) 0
    
    buildRow :: [Int] -> (Int, Int) -> [Int]
    buildRow prevRow (weight, value) =
      [if w < weight 
       then prevRow !! w
       else max (prevRow !! w) (value + prevRow !! (w - weight))
      | w <- [0..capacity]]
