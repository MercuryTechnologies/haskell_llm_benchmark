module GameOfLife (tick) where

-- Function to calculate the next generation of the grid
tick :: [[Int]] -> [[Int]]
tick grid = [[nextState x y | y <- [0..cols-1]] | x <- [0..rows-1]]
  where
    rows = length grid
    cols = if rows == 0 then 0 else length (head grid)

    -- Function to determine the next state of a cell
    nextState :: Int -> Int -> Int
    nextState x y
      | current == 1 && (liveNeighbors == 2 || liveNeighbors == 3) = 1
      | current == 0 && liveNeighbors == 3 = 1
      | otherwise = 0
      where
        current = grid !! x !! y
        liveNeighbors = countLiveNeighbors x y

    -- Function to count live neighbors of a cell
    countLiveNeighbors :: Int -> Int -> Int
    countLiveNeighbors x y = length $ filter (== 1) neighbors
      where
        neighbors = [grid !! nx !! ny | nx <- [x-1..x+1], ny <- [y-1..y+1], inBounds nx ny, (nx, ny) /= (x, y)]

    -- Function to check if a cell is within the grid bounds
    inBounds :: Int -> Int -> Bool
    inBounds x y = x >= 0 && x < rows && y >= 0 && y < cols
