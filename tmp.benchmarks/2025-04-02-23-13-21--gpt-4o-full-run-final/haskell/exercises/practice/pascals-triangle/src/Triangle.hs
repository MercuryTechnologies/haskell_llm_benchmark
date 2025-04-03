module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n pascalTriangle
  where
    pascalTriangle = iterate nextRow [1]
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
