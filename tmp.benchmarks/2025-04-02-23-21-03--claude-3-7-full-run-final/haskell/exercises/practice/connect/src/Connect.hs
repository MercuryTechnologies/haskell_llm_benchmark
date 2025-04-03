module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

-- | Determine the winner of a Connect game
-- Cross (X) wins by connecting left to right
-- Nought (O) wins by connecting top to bottom
winner :: [String] -> Maybe Mark
winner board
  | hasWon Cross = Just Cross
  | hasWon Nought = Just Nought
  | otherwise = Nothing
  where
    hasWon :: Mark -> Bool
    hasWon mark = any (isConnected mark) (startPositions mark)
    
    -- Get all starting positions for a player
    startPositions :: Mark -> [(Int, Int)]
    startPositions Cross = [(r, 0) | r <- [0..height-1], getMarkAt (r, 0) == Just Cross]
    startPositions Nought = [(0, c) | c <- [0..width-1], getMarkAt (0, c) == Just Nought]
    
    -- Check if a starting position connects to the opposite side
    isConnected :: Mark -> (Int, Int) -> Bool
    isConnected mark start = isConnectedDFS mark start []
    
    -- Depth-first search to find a path to the opposite side
    isConnectedDFS :: Mark -> (Int, Int) -> [(Int, Int)] -> Bool
    isConnectedDFS Cross (r, c) visited
      | c >= width - 1 = True  -- Reached right edge (changed from == to >=)
      | otherwise = 
          let validNeighbors = getValidNeighbors Cross (r, c) visited
          in any (\pos -> isConnectedDFS Cross pos ((r, c):visited)) validNeighbors
    isConnectedDFS Nought (r, c) visited
      | r >= height - 1 = True  -- Reached bottom edge (changed from == to >=)
      | otherwise = 
          let validNeighbors = getValidNeighbors Nought (r, c) visited
          in any (\pos -> isConnectedDFS Nought pos ((r, c):visited)) validNeighbors
    
    -- Get valid neighboring positions with the same mark
    getValidNeighbors :: Mark -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    getValidNeighbors mark (r, c) visited =
      filter (\pos -> pos `notElem` visited && getMarkAt pos == Just mark) (getNeighbors (r, c))
    
    -- Get all possible neighboring positions (hex grid)
    getNeighbors :: (Int, Int) -> [(Int, Int)]
    getNeighbors (r, c) = 
      [ (r-1, c), (r-1, c+1),
        (r, c-1), (r, c+1),
        (r+1, c-1), (r+1, c) ]
    
    -- Get the mark at a position, if valid
    getMarkAt :: (Int, Int) -> Maybe Mark
    getMarkAt (r, c)
      | r < 0 || r >= height || c < 0 || c >= width = Nothing
      | otherwise = 
          case getCharAt r c of
            'X' -> Just Cross
            'O' -> Just Nought
            _ -> Nothing
    
    -- Get the character at a position in the board
    getCharAt :: Int -> Int -> Char
    getCharAt r c = 
      let row = board !! r
          -- Account for the indentation in the hex grid
          effectiveCol = c + r
      in if effectiveCol < length row
         then row !! effectiveCol
         else '.'
    
    -- Board dimensions
    height = length board
    width = if height > 0 
            then maximum (map (\r -> length (board !! r) - r) [0..height-1])
            else 0
