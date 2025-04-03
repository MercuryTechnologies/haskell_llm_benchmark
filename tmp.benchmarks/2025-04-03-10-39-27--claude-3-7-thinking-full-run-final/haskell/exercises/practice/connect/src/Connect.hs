module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

-- Parse the board to a clean 2D grid
parseBoard :: [String] -> [[Char]]
parseBoard = map (filter (/= ' '))

-- Find all connected cells using depth-first search
findConnected :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
findConnected board player start = dfs [start] []
  where
    inBounds (r, c) = r >= 0 && r < length board && c >= 0 && c < length (board !! r)
    
    dfs [] visited = visited
    dfs ((r, c):stack) visited
      | not (inBounds (r, c)) = dfs stack visited
      | (r, c) `elem` visited = dfs stack visited
      | board !! r !! c /= player = dfs stack visited
      | otherwise =
          let newVisited = (r, c) : visited
              -- Get all neighbors for the current position
              neighbors = filter inBounds $ adjacent r c
          in dfs (neighbors ++ stack) newVisited
    
    -- Get adjacent positions for a hexagonal grid
    adjacent r c =
      let -- Adjustments for even and odd rows due to the hexagonal grid
          directions = if odd r  -- reversed logic based on how the board is represented
                      then [(-1,0), (-1,1), (0,-1), (0,1), (1,0), (1,1)]
                      else [(-1,-1), (-1,0), (0,-1), (0,1), (1,-1), (1,0)]
      in map (\(dr, dc) -> (r + dr, c + dc)) directions

-- Check if there's a connected path from one edge to the opposite
hasPath :: [[Char]] -> Char -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> Bool
hasPath board player isStartEdge isEndEdge =
  let rows = length board
      -- Find all starting positions on the start edge
      startPositions = [(r, c) | r <- [0..rows-1], c <- [0..length (board !! r)-1], 
                        isStartEdge r c, board !! r !! c == player]
      -- For each starting position, check if it has a path to the end edge
      connectedToEnd start =
        any (\(r, c) -> isEndEdge r c) (findConnected board player start)
  in any connectedToEnd startPositions

winner :: [String] -> Maybe Mark
winner boardStr =
  let board = parseBoard boardStr
      rows = length board
      -- Define edge conditions for players
      isTopEdge r _ = r == 0
      isBottomEdge r _ = r == rows - 1
      isLeftEdge _ c = c == 0
      isRightEdge r c = c == length (board !! r) - 1
      
      -- Check if either player has a winning path
      oWins = hasPath board 'O' isTopEdge isBottomEdge
      xWins = hasPath board 'X' isLeftEdge isRightEdge
  in case (oWins, xWins) of
       (True, False) -> Just Nought
       (False, True) -> Just Cross
       _ -> Nothing
