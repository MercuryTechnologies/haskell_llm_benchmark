module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
    | not (isValidBoard board) = Impossible
    | hasWon 'X' board && hasWon 'O' board = Impossible
    | hasWon 'X' board && xCount > oCount = WinX
    | hasWon 'O' board && xCount == oCount = WinO
    | isDraw board = Draw
    | otherwise = Ongoing
  where
    xCount = length (filter (== 'X') (concat board))
    oCount = length (filter (== 'O') (concat board))

isValidBoard :: [String] -> Bool
isValidBoard board = xCount >= oCount && xCount <= oCount + 1
  where
    xCount = length (filter (== 'X') (concat board))
    oCount = length (filter (== 'O') (concat board))

hasWon :: Char -> [String] -> Bool
hasWon player board = any (all (== player)) (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = [[board !! i !! i | i <- [0..2]], [board !! i !! (2 - i) | i <- [0..2]]]

isDraw :: [String] -> Bool
isDraw board = all (\row -> all (`elem` "XO") row) board && not (hasWon 'X' board) && not (hasWon 'O' board)

transpose :: [String] -> [String]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
