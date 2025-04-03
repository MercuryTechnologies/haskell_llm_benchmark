module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board =
  case validateBoard board of
    Nothing -> Impossible
    Just board' ->
      case checkWin 'X' board' of
        True -> case checkWin 'O' board' of
                  True -> Impossible
                  False -> WinX
        False -> case checkWin 'O' board' of
                   True -> WinO
                   False -> if isBoardFull board'
                              then Draw
                              else Ongoing

validateBoard :: [String] -> Maybe [String]
validateBoard board
  | length board /= 9 = Nothing
  | not (all (\s -> s `elem` ["X", "O", " "]) board) = Nothing
  | otherwise =
      let xCount = length (filter (=="X") board)
          oCount = length (filter (=="O") board)
      in if abs (xCount - oCount) > 1 || oCount > xCount
         then Nothing
         else if (checkWin 'X' board || checkWin 'O' board) && checkWinAfterMove board 'O' then Nothing else Just board

checkWin :: Char -> [String] -> Bool
checkWin player board =
  any (== replicate 3 [player]) (rows ++ cols ++ diags)
  where
    rows = map (\i -> [board !! (i*3 + j) | j <- [0..2]]) [0..2]
    cols = map (\i -> [board !! (j*3 + i) | j <- [0..2]]) [0..2]
    diags = [[board !! i | i <- [0,4,8]], [board !! i | i <- [2,4,6]]]
    replicate n x = map (const (show x)) [1..n]

checkWinAfterMove :: [String] -> Char -> Bool
checkWinAfterMove board player =
  let
    emptyIndices = [i | (x, i) <- zip board [0..], x == " "]
    possibleBoards = map (\i -> replace board i (show player)) emptyIndices
  in
    any (checkWin player) possibleBoards

replace :: [String] -> Int -> String -> [String]
replace xs i x = take i xs ++ [x] ++ drop (i+1) xs

isBoardFull :: [String] -> Bool
isBoardFull board = notElem " " board
