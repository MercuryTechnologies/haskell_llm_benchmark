module Connect (Mark(..), winner) where

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | hasPath 'X' = Just Cross
  | hasPath 'O' = Just Nought
  | otherwise   = Nothing
  where
    size = length board
    hasPath player = any (dfs player) (initialPositions player)

    initialPositions player
      | player == 'X' = [(x, 0) | x <- [0..size-1], board !! x !! 0 == player]
      | player == 'O' = [(0, y) | y <- [0..size-1], board !! 0 !! y == player]
      | otherwise     = []

    dfs player (x, y) = explore [(x, y)] []
      where
        explore [] _ = False
        explore ((cx, cy):stack) visited
          | isWinningPosition player (cx, cy) = True
          | (cx, cy) `elem` visited = explore stack visited
          | otherwise = explore (neighbors player (cx, cy) ++ stack) ((cx, cy):visited)

    isWinningPosition player (x, y)
      | player == 'X' = y == size - 1
      | player == 'O' = x == size - 1
      | otherwise     = False

    neighbors player (x, y) = filter isValidPosition
      [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y+1), (x+1, y-1)]
      where
        isValidPosition (nx, ny) = nx >= 0 && ny >= 0 && nx < size && ny < size && board !! nx !! ny == player
