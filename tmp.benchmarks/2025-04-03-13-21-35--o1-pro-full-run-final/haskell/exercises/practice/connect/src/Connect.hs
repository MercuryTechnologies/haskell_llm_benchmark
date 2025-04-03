module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

-- | Determine the winner of a Hex/Polygon game board.
--   'X' tries to connect left to right.
--   'O' tries to connect top to bottom.
--   Each line in the board input may contain leading spaces; ignore them and split on spaces.
--   '.' indicates an empty cell.
--   Return 'Just Cross' if X has a successful path, 'Just Nought' if O has a successful path, or 'Nothing' if neither has succeeded.
winner :: [String] -> Maybe Mark
winner board =
    let mat = boardMatrix board
    in if checkCross mat
       then Just Cross
       else if checkNought mat
       then Just Nought
       else Nothing

-- Parse lines into a 2D grid of Maybe Mark
boardMatrix :: [String] -> [[Maybe Mark]]
boardMatrix = map (map charToMark . words)
  where
    charToMark "." = Nothing
    charToMark "X" = Just Cross
    charToMark "O" = Just Nought
    charToMark _   = Nothing

-- Check whether 'X' (Cross) forms a path from left to right
checkCross :: [[Maybe Mark]] -> Bool
checkCross mat =
    let h = length mat
        w = if h == 0 then 0 else length (head mat)
        -- Positions in the left column containing X
        leftPositions = [(r, 0) | r <- [0 .. h - 1], w > 0, mat !! r !! 0 == Just Cross]
        goal (r, c) = c == w - 1
    in search leftPositions [] goal mat Cross

-- Check whether 'O' (Nought) forms a path from top to bottom
checkNought :: [[Maybe Mark]] -> Bool
checkNought mat =
    let h = length mat
        w = if h == 0 then 0 else length (head mat)
        -- Positions in the top row containing O
        topPositions = [(0, c) | c <- [0 .. w - 1], h > 0, mat !! 0 !! c == Just Nought]
        goal (r, _) = r == h - 1
    in search topPositions [] goal mat Nought

-- Generalized BFS/DFS search
--    positions: positions to explore
--    visited:   already explored
--    goal:      goal test
--    mat:       board
--    mark:      Mark we're searching for
search :: [(Int, Int)]        -- positions
       -> [(Int, Int)]        -- visited
       -> ((Int, Int) -> Bool) -- goal
       -> [[Maybe Mark]]      -- board
       -> Mark                -- mark to match
       -> Bool
search [] _ _ _ _ = False
search (p:ps) visited goal mat mark
    | goal p    = True
    | p `elem` visited = search ps visited goal mat mark
    | otherwise =
        let visited' = p : visited
            ns = filter (\(r, c) -> inBounds r c mat && mat !! r !! c == Just mark)
                 (neighbors p (length mat) (length (head mat)))
        in search (ps ++ ns) visited' goal mat mark

-- Return all neighbor coordinates in a hex grid
neighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbors (i, j) h w =
    filter inRange
      [ (i - 1, j), (i - 1, j + 1),
        (i,     j - 1), (i,     j + 1),
        (i + 1, j - 1), (i + 1, j) ]
  where
    inRange (r, c) = r >= 0 && r < h && c >= 0 && c < w

-- Ensure indices are in-bounds
inBounds :: Int -> Int -> [[Maybe Mark]] -> Bool
inBounds r c mat =
    r >= 0 && r < length mat && c >= 0 && c < length (head mat)
