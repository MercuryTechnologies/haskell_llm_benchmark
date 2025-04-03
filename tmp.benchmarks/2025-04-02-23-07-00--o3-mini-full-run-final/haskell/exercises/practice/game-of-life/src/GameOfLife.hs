module GameOfLife (tick) where

-- | Compute the next generation of Conway's Game of Life.
tick :: [[Int]] -> [[Int]]
tick board = [ [ nextCell i j | j <- [0..(width - 1)] ] | i <- [0..(height - 1)] ]
  where
    height = length board
    width  = if null board then 0 else length (head board)

    -- Get the value of the cell at (i,j). Out-of-bound cells are considered dead.
    getCell :: Int -> Int -> Int
    getCell i j
      | i < 0 || i >= height = 0
      | j < 0 || j >= width  = 0
      | otherwise            = (board !! i) !! j

    -- Count live neighbors of the cell at (i, j).
    liveNeighbors :: Int -> Int -> Int
    liveNeighbors i j = sum [ getCell (i + di) (j + dj)
                            | di <- [-1, 0, 1]
                            , dj <- [-1, 0, 1]
                            , not (di == 0 && dj == 0)
                            ]

    -- Apply Conway's Game of Life rules:
    -- 1. Any live cell with two or three live neighbors lives on.
    -- 2. Any dead cell with exactly three live neighbors becomes a live cell.
    -- 3. All other cells die or remain dead.
    nextCell :: Int -> Int -> Int
    nextCell i j =
      let cell = getCell i j
          neighbors = liveNeighbors i j
      in case cell of
           1 -> if neighbors == 2 || neighbors == 3 then 1 else 0
           0 -> if neighbors == 3 then 1 else 0
           _ -> 0
