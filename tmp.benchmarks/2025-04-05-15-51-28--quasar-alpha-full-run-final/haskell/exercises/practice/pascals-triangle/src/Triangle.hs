module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n pascalsTriangle
  where
    pascalsTriangle = iterate nextRow [1]
    nextRow prev = zipWith (+) ([0] ++ prev) (prev ++ [0])
