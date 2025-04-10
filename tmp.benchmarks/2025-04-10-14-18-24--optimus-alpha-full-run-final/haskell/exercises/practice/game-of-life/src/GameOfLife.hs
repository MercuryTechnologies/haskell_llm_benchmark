module GameOfLife (tick) where

-- | Given a grid of 0s and 1s, compute the next generation according to Conway's Game of Life rules.
tick :: [[Int]] -> [[Int]]
tick grid = [ [ nextCellState r c | c <- [0..cols-1] ] | r <- [0..rows-1] ]
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)

    -- Get the value of a cell, or 0 if out of bounds
    cellAt :: Int -> Int -> Int
    cellAt x y
      | x < 0 || x >= rows = 0
      | y < 0 || y >= cols = 0
      | otherwise = (grid !! x) !! y

    -- Count live neighbors for a cell at (r, c)
    liveNeighbors :: Int -> Int -> Int
    liveNeighbors r c =
      sum [ cellAt (r+dr) (c+dc)
          | dr <- [-1,0,1]
          , dc <- [-1,0,1]
          , not (dr == 0 && dc == 0)
          ]

    -- Compute the next state for a cell at (r, c)
    nextCellState :: Int -> Int -> Int
    nextCellState r c =
      let alive = cellAt r c == 1
          neighbors = liveNeighbors r c
      in case (alive, neighbors) of
           (True, n) | n == 2 || n == 3 -> 1
           (False, 3)                   -> 1
           _                            -> 0
