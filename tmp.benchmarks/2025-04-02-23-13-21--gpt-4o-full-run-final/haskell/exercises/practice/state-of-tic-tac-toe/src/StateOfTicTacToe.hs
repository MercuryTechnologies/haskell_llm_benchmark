module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
    | not (validBoard board) = Impossible
    | hasWinner 'X' board = WinX
    | hasWinner 'O' board = WinO
    | fullBoard board = Draw
    | otherwise = Ongoing

validBoard :: [String] -> Bool
validBoard board = 
    let xCount = count 'X' board
        oCount = count 'O' board
    in xCount == oCount || xCount == oCount + 1 && not (hasWinner 'X' board && hasWinner 'O' board)

count :: Char -> [String] -> Int
count player = sum . map (length . filter (== player))

hasWinner :: Char -> [String] -> Bool
hasWinner player board = any (all (== player)) (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = [ [board !! i !! i | i <- [0..2]], [board !! i !! (2-i) | i <- [0..2]] ]

fullBoard :: [String] -> Bool
fullBoard = all (all (/= ' '))

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)
