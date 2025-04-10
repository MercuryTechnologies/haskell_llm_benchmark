module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n pascals
  where
    pascals = iterate nextRow [1]
    nextRow prev = zipWith (+) ([0] ++ prev) (prev ++ [0])
