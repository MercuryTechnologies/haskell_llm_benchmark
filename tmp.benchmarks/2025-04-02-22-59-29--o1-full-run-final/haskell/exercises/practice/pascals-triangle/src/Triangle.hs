module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0 = []
  | otherwise = take n (iterate next [1])
  where
    next row = zipWith (+) (0 : row) (row ++ [0])
