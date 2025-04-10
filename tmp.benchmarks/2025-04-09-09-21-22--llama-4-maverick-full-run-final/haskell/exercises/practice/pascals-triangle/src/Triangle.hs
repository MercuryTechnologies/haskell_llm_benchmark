module Triangle (rows) where

-- Generate the nth row of Pascal's triangle
row :: Int -> [Integer]
row 1 = [1]
row n = zipWith (+) (0 : row (n-1)) (row (n-1) ++ [0])

rows :: Int -> [[Integer]]
rows n = map row [1..n]
