module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

type Board = [[Char]]

-- | Convert the input list of strings to a 3x3 board, or throw error if invalid
parseBoard :: [String] -> Board
parseBoard rows
  | length rows /= 3 = error "Board must have exactly 3 rows"
  | any (\r -> length r /= 3) rows = error "Each row must have exactly 3 columns"
  | any (\c -> c /= 'X' && c /= 'O' && c /= ' ') (concat rows) = error "Invalid character on board"
  | otherwise = rows

-- | Count Xs and Os
countXO :: Board -> (Int, Int)
countXO board = (count 'X', count 'O')
  where
    count c = length $ filter (== c) (concat board)

-- | All possible lines (rows, columns, diagonals)
allLines :: Board -> [[Char]]
allLines b = rows ++ cols ++ diags
  where
    rows = b
    cols = transpose b
    diags = [[b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2],
             [b !! 0 !! 2, b !! 1 !! 1, b !! 2 !! 0]]

-- | Check if a player has won
hasWon :: Char -> Board -> Bool
hasWon c board = any (all (== c)) (allLines board)

-- | Check if the board is full
isFull :: Board -> Bool
isFull = all (/= ' ') . concat

-- | Check for impossible states
isImpossible :: Board -> Bool
isImpossible board =
  let (xCount, oCount) = countXO board
      xWin = hasWon 'X' board
      oWin = hasWon 'O' board
  in
    -- X cannot play after O, and O cannot play after X
    (oCount > xCount) ||
    (xCount > oCount + 1) ||
    -- Both players cannot win at the same time
    (xWin && oWin) ||
    -- O cannot win if X has more moves
    (oWin && xCount /= oCount) ||
    -- X cannot win if X and O have the same number of moves
    (xWin && xCount == oCount)

gameState :: [String] -> GameState
gameState input =
  let board = parseBoard input
      (xCount, oCount) = countXO board
      xWin = hasWon 'X' board
      oWin = hasWon 'O' board
  in
    if isImpossible board
      then Impossible
    else if xWin
      then WinX
    else if oWin
      then WinO
    else if isFull board
      then Draw
    else Ongoing
