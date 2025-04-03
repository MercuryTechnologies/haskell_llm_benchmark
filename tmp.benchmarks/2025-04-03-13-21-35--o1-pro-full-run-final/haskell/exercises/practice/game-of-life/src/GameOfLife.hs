module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick matrix =
  let
    rows = length matrix
    cols = if null matrix then 0 else length (head matrix)

    countNeighbors r c =
      sum [ matrix !! nr !! nc
          | nr <- [r-1..r+1]
          , nc <- [c-1..c+1]
          , (nr, nc) /= (r, c)
          , nr >= 0, nr < rows
          , nc >= 0, nc < cols
          ]

    nextCellValue r c =
      let alive = matrix !! r !! c
          n      = countNeighbors r c
      in case alive of
           1 -> if n == 2 || n == 3 then 1 else 0
           0 -> if n == 3 then 1 else 0
           _ -> 0  -- Fallback in case the input is neither 0 or 1

  in [ [ nextCellValue r c | c <- [0..cols-1] ]
     | r <- [0..rows-1]
     ]
