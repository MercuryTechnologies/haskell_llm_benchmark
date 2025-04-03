module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n (iterate nextRow [1])
  where
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
