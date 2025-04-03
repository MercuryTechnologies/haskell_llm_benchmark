module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- Helper function to check if a player has won by completing a line
checkWin :: Char -> [String] -> Bool
checkWin player board =
    any allPlayer rows || any allPlayer cols || any allPlayer diags
  where
    -- Check rows
    rows = board
    -- Check columns by transposing the board
    cols = transpose board
    -- Check the two main diagonals
    diag1 = [board !! i !! i | i <- [0..2]]
    diag2 = [board !! i !! (2-i) | i <- [0..2]]
    diags = [diag1, diag2]
    -- Helper to check if all elements in a list match the player's mark
    allPlayer = all (== player)

-- Main function to determine the game state
gameState :: [String] -> GameState
gameState board =
    let allChars = concat board
        numX = length $ filter (== 'X') allChars
        numO = length $ filter (== 'O') allChars
        numEmpty = length $ filter (== ' ') allChars -- Assuming empty cells are spaces ' '

        xWins = checkWin 'X' board
        oWins = checkWin 'O' board

    in
        -- Rule: X starts, so numX must be equal to numO or numO + 1.
        if numX < numO || numX > numO + 1 then Impossible
        -- Rule: Cannot have both players winning simultaneously.
        else if xWins && oWins then Impossible
        -- Rule: If X wins, the number of X's must be one more than the number of O's.
        -- (X must have made the last, winning move).
        else if xWins && numX == numO then Impossible
        -- Rule: If O wins, the number of O's must be equal to the number of X's.
        -- (O must have made the last, winning move).
        else if oWins && numX > numO then Impossible
        -- If valid so far, determine the final state:
        -- Check for wins first
        else if xWins then WinX
        else if oWins then WinO
        -- If no one has won, check if the board is full (draw)
        else if numEmpty == 0 then Draw
        -- Otherwise, the game is still ongoing
        else Ongoing
