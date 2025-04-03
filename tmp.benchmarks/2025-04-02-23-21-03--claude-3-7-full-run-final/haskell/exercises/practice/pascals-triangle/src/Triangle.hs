module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n pascalRows
  where
    pascalRows = [1] : map nextRow pascalRows
    nextRow row = zipWith (+) (0 : row) (row ++ [0])
