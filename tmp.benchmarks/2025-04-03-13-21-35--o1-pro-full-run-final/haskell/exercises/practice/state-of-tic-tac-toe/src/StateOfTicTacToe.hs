module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible
  deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board =
    if not validDimensions
       then Impossible
       else
         let
             countX = length (filter (=='X') (concat board))
             countO = length (filter (=='O') (concat board))
         in
             if countO > countX || countX > countO + 1
                then Impossible
                else
                    let xWins = checkWin 'X' board
                        oWins = checkWin 'O' board
                    in
                        case (xWins, oWins) of
                            (True, True)  -> Impossible
                            (True, False) ->
                                if countX == countO + 1
                                   then WinX
                                   else Impossible
                            (False, True) ->
                                if countO == countX
                                   then WinO
                                   else Impossible
                            (False, False) ->
                                if (countX + countO == 9)
                                   then Draw
                                   else Ongoing
  where
    validDimensions = length board == 3 && all (\row -> length row == 3) board

    -- Check if a given player (c) has a winning line
    checkWin :: Char -> [String] -> Bool
    checkWin c b =
        let
           rows = b
           cols = transpose b
           diags = [ [b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2]
                   , [b !! 0 !! 2, b !! 1 !! 1, b !! 2 !! 0]
                   ]
           linesToCheck = rows ++ cols ++ diags
        in any (\line -> all (== c) line) linesToCheck
