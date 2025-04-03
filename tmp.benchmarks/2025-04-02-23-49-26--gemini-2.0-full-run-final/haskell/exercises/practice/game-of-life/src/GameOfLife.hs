module GameOfLife (tick) where

-- Counts the number of live neighbors for a given cell
countLiveNeighbors :: [[Int]] -> Int -> Int -> Int
countLiveNeighbors grid row col = sum $ map (neighborValue grid row col) neighbors
  where
    neighbors = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    neighborValue :: [[Int]] -> Int -> Int -> (Int, Int) -> Int
    neighborValue grid row col (rowOffset, colOffset) =
      let
        neighborRow = row + rowOffset
        neighborCol = col + colOffset
      in
        if neighborRow >= 0 && neighborRow < numRows && neighborCol >= 0 && neighborCol < numCols
          then grid !! neighborRow !! neighborCol
          else 0
    numRows = length grid
    numCols = length (head grid)

-- Applies the rules of Conway's Game of Life to a single cell
applyRules :: [[Int]] -> Int -> Int -> Int
applyRules grid row col =
  let
    liveNeighbors = countLiveNeighbors grid row col
    cellValue = grid !! row !! col
  in
    if cellValue == 1
      then
        if liveNeighbors == 2 || liveNeighbors == 3
          then 1
          else 0
      else
        if liveNeighbors == 3
          then 1
          else 0

-- Generates the next generation of the game
tick :: [[Int]] -> [[Int]]
tick grid =
  let
    numRows = length grid
    numCols = length (head grid)
  in
    [ [applyRules grid row col | col <- [0 .. numCols - 1]] | row <- [0 .. numRows - 1]]
