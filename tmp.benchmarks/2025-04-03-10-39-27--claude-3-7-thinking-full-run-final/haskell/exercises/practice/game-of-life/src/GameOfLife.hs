module GameOfLife (tick) where

-- | Advance the game state by one tick, applying Conway's Game of Life rules
tick :: [[Int]] -> [[Int]]
tick [] = []  -- Handle empty grid
tick grid = [[nextCellState grid i j | j <- [0..cols-1]] | i <- [0..rows-1]]
  where
    rows = length grid
    cols = if rows > 0 then length (head grid) else 0

-- | Determine the next state of a cell based on its neighbors
nextCellState :: [[Int]] -> Int -> Int -> Int
nextCellState grid i j = applyRules (grid !! i !! j) (countLiveNeighbors grid i j)

-- | Count the number of live neighbors for a cell at position (i, j)
countLiveNeighbors :: [[Int]] -> Int -> Int -> Int
countLiveNeighbors grid i j = sum [getValue grid i' j' | i' <- [i-1..i+1], j' <- [j-1..j+1],
                                  not (i' == i && j' == j)]
  where
    getValue g i' j'
      | i' < 0 || i' >= length g = 0
      | j' < 0 || j' >= length (head g) = 0
      | otherwise = g !! i' !! j'

-- | Apply the Game of Life rules to a cell
applyRules :: Int -> Int -> Int
applyRules cell liveNeighbors
    | cell == 1 && (liveNeighbors == 2 || liveNeighbors == 3) = 1  -- Live cell with 2 or 3 live neighbors lives on
    | cell == 0 && liveNeighbors == 3 = 1                          -- Dead cell with exactly 3 live neighbors becomes live
    | otherwise = 0                                                -- All other cells die or stay dead
