module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

countSymbol :: Char -> [String] -> Int
countSymbol symbol board = sum [count row | row <- board]
  where
    count row = length [c | c <- row, c == symbol]

hasWon :: Char -> [String] -> Bool
hasWon symbol board = any (all (== symbol)) (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = [[board !! i !! i | i <- [0..2]], [board !! i !! (2-i) | i <- [0..2]]]

gameState :: [String] -> GameState
gameState board = 
  let xCount = countSymbol 'X' board
      oCount = countSymbol 'O' board
      xWon = hasWon 'X' board
      oWon = hasWon 'O' board
  in
    if abs (xCount - oCount) > 1 || (xWon && oWon) then Impossible
    else if xWon then
      if xCount - oCount == 1 && not oWon then WinX else Impossible
    else if oWon then
      if xCount == oCount && not xWon then WinO else Impossible
    else if xCount + oCount == 9 then Draw
    else if xCount - oCount == 0 || xCount - oCount == 1 then Ongoing
    else Impossible
