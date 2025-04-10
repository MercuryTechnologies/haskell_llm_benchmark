module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n = go n 1
  where
    go 0 _ = []
    go 1 start = [[start]]
    go size start =
      let
        -- Top row
        top = [start .. start + size - 1]
        -- Right column (excluding the top cell)
        right = [start + size .. start + 2 * size - 2]
        -- Bottom row (excluding the rightmost cell), reversed
        bottom = reverse [start + 2 * size - 1 .. start + 3 * size - 3]
        -- Left column (excluding the top and bottom cells), reversed
        left = reverse [start + 3 * size - 2 .. start + 4 * size - 5]
        -- Recursively build the inner spiral
        inner = go (size - 2) (start + 4 * size - 4)
        -- Build the middle rows
        middleRows =
          [ [l] ++ (if null inner then [] else innerRow) ++ [r]
          | (l, innerRow, r) <- zip3 left (if null inner then repeat [] else inner) right
          ]
      in
        [top]
        ++ middleRows
        ++ [bottom | size > 1]
