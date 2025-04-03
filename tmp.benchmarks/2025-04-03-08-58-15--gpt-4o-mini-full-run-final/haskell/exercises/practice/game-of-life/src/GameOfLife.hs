module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [[nextState x y | y <- [0..(length row - 1)]] | (row, x) <- zip grid [0..(length grid - 1)]]
  where
    nextState x y = let liveNeighbors = countLiveNeighbors x y
                    in if (grid !! x !! y) == 1
                       then if liveNeighbors == 2 || liveNeighbors == 3 then 1 else 0
                       else if liveNeighbors == 3 then 1 else 0

    countLiveNeighbors x y = sum [getCell (x + dx) (y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

    getCell x y
      | x < 0 || x >= length grid || y < 0 || y >= length (head grid) = 0
      | otherwise = grid !! x !! y
