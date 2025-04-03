module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible
  deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board =
    let flattened = concat board
    in if length flattened /= 9
       then
         Impossible
       else
         let countX = length (filter (=='X') flattened)
             countO = length (filter (=='O') flattened)
         in if not (countX == countO || countX == countO + 1)
            then
              Impossible
            else
              let winningLines = [ (0,1,2), (3,4,5), (6,7,8)
                                 , (0,3,6), (1,4,7), (2,5,8)
                                 , (0,4,8), (2,4,6) ]
                  lineOfChars (a,b,c) = [ flattened !! a
                                        , flattened !! b
                                        , flattened !! c ]
                  isWin c = any (\(x,y,z) -> lineOfChars (x,y,z) == [c,c,c]) winningLines
                  xWin = isWin 'X'
                  oWin = isWin 'O'
              in if xWin && oWin
                 then
                   Impossible
                 else if xWin
                   then
                     if countX == countO
                     then Impossible
                     else WinX
                   else if oWin
                     then
                       if countX > countO
                       then Impossible
                       else WinO
                     else
                       if countX + countO == 9
                       then Draw
                       else Ongoing
