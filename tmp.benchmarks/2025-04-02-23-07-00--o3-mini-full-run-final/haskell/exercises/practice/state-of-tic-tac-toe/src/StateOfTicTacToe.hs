module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | length board /= 3 || any ((/= 3) . length) board = error "Invalid board: board must be 3x3"
  | countO > countX = error "Invalid board: O went twice"
  | countX > countO + 1 = error "Invalid board: X went twice"
  | winX && winO = error "Invalid board: both players cannot win"
  | winX && countX /= countO + 1 = error "Invalid board: X wins but move count invalid"
  | winO && countX /= countO = error "Invalid board: O wins but move count invalid"
  | winX && playedAfterWin 'X' board = error "Invalid board: moves played after game was won"
  | winO && playedAfterWin 'O' board = error "Invalid board: moves played after game was won"
  | winX = WinX
  | winO = WinO
  | countX + countO == 9 = Draw
  | otherwise = Ongoing
  where
    countX = sum [ length (filter (== 'X') row) | row <- board ]
    countO = sum [ length (filter (== 'O') row) | row <- board ]
    winX = hasWin 'X' board
    winO = hasWin 'O' board

hasWin :: Char -> [String] -> Bool
hasWin p board = any (all (== p)) (rows ++ cols ++ dias)
  where
    rows = board
    cols = transpose board
    dias = [ [board !! i !! i | i <- [0..2]]
           , [board !! i !! (2-i) | i <- [0..2]]
           ]

winningLinePositions :: [[(Int, Int)]]
winningLinePositions =
  let rows = [[(r, c) | c <- [0..2]] | r <- [0..2]]
      cols = [[(r, c) | r <- [0..2]] | c <- [0..2]]
      dias = [[(i, i) | i <- [0..2]], [(i, 2-i) | i <- [0..2]]]
  in rows ++ cols ++ dias

playedAfterWin :: Char -> [String] -> Bool
playedAfterWin p board =
  -- For a board that shows a win, check if there is at least one candidate removal
  -- (i.e. a removal of a mark from a winning line) that causes the win to vanish,
  -- implying that the win was achieved with that move and no extra moves were played.
  not (any candidateFound winningLinePositions)
  where
    winningLine :: [(Int, Int)] -> Bool
    winningLine line = all (\(r, c) -> board !! r !! c == p) line

    candidateFound :: [(Int, Int)] -> Bool
    candidateFound line
      | not (winningLine line) = False
      | otherwise = any (\(r, c) -> not (hasWin p (removeMark board r c))) line

removeMark :: [String] -> Int -> Int -> [String]
removeMark board r c =
  let row = board !! r
      newRow = take c row ++ " " ++ drop (c+1) row
  in take r board ++ [newRow] ++ drop (r+1) board
