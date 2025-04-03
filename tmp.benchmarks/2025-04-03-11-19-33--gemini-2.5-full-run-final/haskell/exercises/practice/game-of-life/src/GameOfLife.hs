module GameOfLife (tick) where

import Data.List (length) -- Import length explicitly or rely on Prelude

tick :: [[Int]] -> [[Int]]
tick grid
  -- Handle empty or invalid grids
  | null grid || null (head grid) = []
  | otherwise =
      let height = length grid
          width = length (head grid)

          -- Safely get cell state from the *original* grid.
          -- Returns 0 (dead) if coordinates are out of bounds.
          getCellState :: Int -> Int -> Int
          getCellState r c
            | r < 0 || r >= height || c < 0 || c >= width = 0
            | otherwise = (grid !! r) !! c

          -- Count live neighbors for cell (r, c) by checking all 8 adjacent cells.
          countLiveNeighbors :: Int -> Int -> Int
          countLiveNeighbors r c =
            sum [ getCellState (r + dr) (c + dc)
                | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0) -- Iterate over relative neighbor coordinates, excluding the cell itself
                ]

          -- Apply Conway's rules to determine the next state of a cell.
          -- Takes the current state (0 or 1) and the count of live neighbors.
          applyRules :: Int -> Int -> Int
          applyRules 1 neighbors -- Current cell is alive
            | neighbors == 2 || neighbors == 3 = 1 -- Survives with 2 or 3 neighbors
            | otherwise                        = 0 -- Dies otherwise (underpopulation or overpopulation)
          applyRules 0 neighbors -- Current cell is dead
            | neighbors == 3                   = 1 -- Becomes alive with exactly 3 neighbors (reproduction)
            | otherwise                        = 0 -- Stays dead otherwise
          applyRules state _ = error ("Invalid cell state encountered: " ++ show state) -- Should only be 0 or 1

          -- Calculate the next state for the cell at coordinates (r, c).
          calculateNextState :: Int -> Int -> Int
          calculateNextState r c =
            let currentState = (grid !! r) !! c -- Get the state from the original grid (guaranteed to be in bounds here)
                liveNeighbors = countLiveNeighbors r c
            in applyRules currentState liveNeighbors

      -- Generate the new grid by applying calculateNextState to each cell coordinate.
      in [ [ calculateNextState r c | c <- [0..width-1] ] | r <- [0..height-1] ]
