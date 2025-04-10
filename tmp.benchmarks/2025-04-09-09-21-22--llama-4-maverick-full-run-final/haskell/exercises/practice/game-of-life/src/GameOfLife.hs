module GameOfLife (tick) where

-- Count the number of live neighbors for a cell at position (i, j) in the grid
countLiveNeighbors :: [[Int]] -> Int -> Int -> Int
countLiveNeighbors grid i j = sum [getCell grid (i+x) (j+y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]
  where
    getCell grid i j = if i < 0 || j < 0 || i >= length grid || j >= length (head grid) then 0 else grid !! i !! j

-- Apply the rules of the game to a cell based on its current state and the number of live neighbors
nextCellState :: Int -> Int -> Int
nextCellState cell liveNeighbors
  | cell == 1 && (liveNeighbors == 2 || liveNeighbors == 3) = 1
  | cell == 0 && liveNeighbors == 3 = 1
  | otherwise = 0

-- Update the state of each cell in the grid
tick :: [[Int]] -> [[Int]]
tick grid = [[nextCellState (grid !! i !! j) (countLiveNeighbors grid i j) | j <- [0..length (head grid) - 1]] | i <- [0..length grid - 1]]
