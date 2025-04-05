module GameOfLife (tick) where

-- Helper function to count live neighbors
countLiveNeighbors :: [[Int]] -> Int -> Int -> Int
countLiveNeighbors board x y = sum [board !! nx !! ny | nx <- [x-1..x+1], 
                                                       ny <- [y-1..y+1],
                                                       nx /= x || ny /= y,
                                                       nx >= 0,
                                                       ny >= 0,
                                                       nx < length board,
                                                       ny < length (head board)]

-- Apply game rules to a single cell
applyRules :: [[Int]] -> Int -> Int -> Int
applyRules board x y = 
    let liveNeighbors = countLiveNeighbors board x y
        current = board !! x !! y
    in case current of
        1 -> if liveNeighbors `elem` [2,3] then 1 else 0
        _ -> if liveNeighbors == 3 then 1 else 0

-- Generate next generation
tick :: [[Int]] -> [[Int]]
tick board = 
    [[applyRules board x y | y <- [0..length (head board) - 1]] 
                          | x <- [0..length board - 1]]
