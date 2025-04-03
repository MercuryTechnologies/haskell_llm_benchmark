module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = map generateRow [0..n-1]
  where
    generateRow 0 = [1]
    generateRow r = [1] ++ zipWith (+) prevRow (tail prevRow) ++ [1]
      where prevRow = rows (r - 1)
