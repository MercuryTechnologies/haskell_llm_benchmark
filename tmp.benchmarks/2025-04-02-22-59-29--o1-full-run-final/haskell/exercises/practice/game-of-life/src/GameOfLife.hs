module GameOfLife (tick) where

-- | Apply one generation of Conway's Game of Life rules to a matrix of 0s and 1s.
--   1 represents alive, 0 represents dead.
tick :: [[Int]] -> [[Int]]
tick board =
    let rows = length board
        cols = if null board then 0 else length (head board)
    in [ [ nextCellState r c | c <- [0..cols-1] ]
       | r <- [0..rows-1] ]
  where
    nextCellState r c =
      let cell = board !! r !! c
          neighbors = countNeighbors r c
      in case cell of
           1 -> if neighbors == 2 || neighbors == 3 then 1 else 0
           0 -> if neighbors == 3 then 1 else 0

    countNeighbors r c =
      sum [ board !! nr !! nc
          | nr <- [r-1..r+1]
          , nc <- [c-1..c+1]
          , (nr, nc) /= (r, c)
          , nr >= 0, nr < length board
          , nc >= 0, nc < (if null board then 0 else length (head board))
          ]
