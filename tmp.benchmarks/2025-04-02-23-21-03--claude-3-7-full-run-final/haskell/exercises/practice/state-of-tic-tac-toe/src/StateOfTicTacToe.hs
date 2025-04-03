module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (validBoard board) = Impossible
  | hasWon 'X' board && hasWon 'O' board = Impossible
  | hasWon 'X' board && countPieces 'O' board >= countPieces 'X' board = Impossible
  | hasWon 'O' board && countPieces 'X' board > countPieces 'O' board = Impossible
  | hasWon 'X' board = WinX
  | hasWon 'O' board = WinO
  | isFull board = Draw
  | otherwise = Ongoing

-- Check if the board is valid based on turn order
validBoard :: [String] -> Bool
validBoard board =
  let xCount = countPieces 'X' board
      oCount = countPieces 'O' board
  in xCount >= oCount && xCount - oCount <= 1

-- Count the number of a specific piece on the board
countPieces :: Char -> [String] -> Int
countPieces piece = sum . map (length . filter (== piece))

-- Check if the board is full
isFull :: [String] -> Bool
isFull board = countPieces 'X' board + countPieces 'O' board == 9

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon player board = 
  any (all (== player)) rows || 
  any (all (== player)) cols || 
  all (== player) diag1 || 
  all (== player) diag2
  where
    rows = [getRow i board | i <- [0..2]]
    cols = [getCol i board | i <- [0..2]]
    diag1 = [board !! i !! i | i <- [0..2]]
    diag2 = [board !! i !! (2-i) | i <- [0..2]]

-- Get a row from the board
getRow :: Int -> [String] -> [Char]
getRow i board = board !! i

-- Get a column from the board
getCol :: Int -> [String] -> [Char]
getCol i board = [row !! i | row <- board]
