module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board = 
    let
        -- Count 'X's and 'O's
        countX = sum [length (filter (== 'X') row) | row <- board]
        countO = sum [length (filter (== 'O') row) | row <- board]
        
        -- Check turn order
        validTurnOrder = countX == countO || countX == countO + 1
        
        -- Check for winners
        winnerX = hasWon 'X' board
        winnerO = hasWon 'O' board
        
        -- Check for invalid scenarios
        multipleWinners = winnerX && winnerO
        wrongWinnerTurn = (winnerX && countX /= countO + 1) || (winnerO && countX /= countO)
        invalid = not validTurnOrder || multipleWinners || wrongWinnerTurn
        
        -- Determine game state
        isFull = not (any (any (\cell -> cell /= 'X' && cell /= 'O')) board)
    in
        if invalid then Impossible
        else if winnerX then WinX
        else if winnerO then WinO
        else if isFull then Draw
        else Ongoing

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon player board =
    -- Check rows
    any (all (== player)) board ||
    -- Check columns
    any (\col -> all (\row -> row !! col == player) board) [0..2] ||
    -- Check diagonals
    (board !! 0 !! 0 == player && board !! 1 !! 1 == player && board !! 2 !! 2 == player) ||
    (board !! 0 !! 2 == player && board !! 1 !! 1 == player && board !! 2 !! 0 == player)
