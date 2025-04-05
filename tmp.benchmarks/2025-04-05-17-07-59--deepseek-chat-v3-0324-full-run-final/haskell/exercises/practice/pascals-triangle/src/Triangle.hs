module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n $ iterate nextRow [1]
  where
    nextRow xs = zipWith (+) (0:xs) (xs ++ [0])
