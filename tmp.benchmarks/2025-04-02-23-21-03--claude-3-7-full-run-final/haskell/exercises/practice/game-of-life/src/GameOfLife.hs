module GameOfLife (tick) where

-- | Advances the game state by one generation according to Conway's Game of Life rules:
-- 1. Any live cell with 2 or 3 live neighbors survives
-- 2. Any dead cell with exactly 3 live neighbors becomes alive
-- 3. All other cells die or stay dead
tick :: [[Int]] -> [[Int]]
tick grid = [[nextCellState grid row col | col <- [0..cols-1]] | row <- [0..rows-1]]
  where
    rows = length grid
    cols = if rows > 0 then length (head grid) else 0

-- | Determines the next state of a cell based on its current state and neighbors
nextCellState :: [[Int]] -> Int -> Int -> Int
nextCellState grid row col
  | isAlive && (liveNeighbors == 2 || liveNeighbors == 3) = 1  -- Live cell with 2 or 3 neighbors survives
  | not isAlive && liveNeighbors == 3 = 1                      -- Dead cell with exactly 3 neighbors becomes alive
  | otherwise = 0                                              -- All other cells die or stay dead
  where
    isAlive = getCellState grid row col == 1
    liveNeighbors = countLiveNeighbors grid row col

-- | Gets the state of a cell (1 for alive, 0 for dead)
-- Returns 0 for cells outside the grid boundaries
getCellState :: [[Int]] -> Int -> Int -> Int
getCellState grid row col
  | row >= 0 && row < rows && col >= 0 && col < cols = grid !! row !! col
  | otherwise = 0
  where
    rows = length grid
    cols = if rows > 0 then length (head grid) else 0

-- | Counts the number of live neighbors around a cell
countLiveNeighbors :: [[Int]] -> Int -> Int -> Int
countLiveNeighbors grid row col = sum [getCellState grid (row+dr) (col+dc) | 
                                       dr <- [-1, 0, 1], 
                                       dc <- [-1, 0, 1], 
                                       not (dr == 0 && dc == 0)]
