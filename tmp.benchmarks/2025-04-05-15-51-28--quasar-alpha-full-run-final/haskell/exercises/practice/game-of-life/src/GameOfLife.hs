module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick grid = [ [ nextState (r, c) | c <- [0..cols-1] ] | r <- [0..rows-1] ]
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)

    getCell (r, c)
      | r < 0 || r >= rows = 0
      | c < 0 || c >= cols = 0
      | otherwise = (grid !! r) !! c

    liveNeighbors (r, c) = sum [ getCell (r + dr, c + dc)
                               | dr <- [-1, 0, 1]
                               , dc <- [-1, 0, 1]
                               , (dr, dc) /= (0, 0) ]

    nextState pos =
      let cell = getCell pos
          neighbors = liveNeighbors pos
      in case cell of
           1 -> if neighbors == 2 || neighbors == 3 then 1 else 0
           0 -> if neighbors == 3 then 1 else 0
           _ -> 0
