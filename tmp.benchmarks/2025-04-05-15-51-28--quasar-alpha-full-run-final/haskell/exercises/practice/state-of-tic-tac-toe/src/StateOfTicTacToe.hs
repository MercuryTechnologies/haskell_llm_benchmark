module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (validBoardSize board) = error "Invalid board size"
  | not (all validRow board) = error "Invalid characters on board"
  | invalidTurnOrder = Impossible
  | multipleWinners = Impossible
  | winX = WinX
  | winO = WinO
  | full = Draw
  | otherwise = Ongoing
  where
    validBoardSize b = length b == 3 && all (\row -> length row == 3) b
    validRow row = all (`elem` "XO ") row

    flatBoard = concat board
    countX = length $ filter (== 'X') flatBoard
    countO = length $ filter (== 'O') flatBoard

    invalidTurnOrder = not (countX == countO || countX == countO + 1)

    rows = board
    cols = transpose board
    diags = [ [board !! i !! i | i <- [0..2]]
            , [board !! i !! (2 - i) | i <- [0..2]] ]

    linesToCheck = rows ++ cols ++ diags

    winX = any (all (== 'X')) linesToCheck
    winO = any (all (== 'O')) linesToCheck

    multipleWinners = (winX && winO) ||
                      (winX && countX == countO) ||
                      (winO && countX > countO)

    full = all (/= ' ') flatBoard
