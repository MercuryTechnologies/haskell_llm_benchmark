module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  let
    emptyBoard = replicate 8 (replicate 8 '_')
    board = case white of
      Just (rowW, colW) -> case black of
        Just (rowB, colB) -> updateBoard emptyBoard rowW colW 'W' rowB colB 'B'
        Nothing -> updateBoard emptyBoard rowW colW 'W' (-1) (-1) '_'
      Nothing -> case black of
        Just (rowB, colB) -> updateBoard emptyBoard (-1) (-1) '_' rowB colB 'B'
        Nothing -> updateBoard emptyBoard (-1) (-1) '_' (-1) (-1) '_'
    rows = map (unwords . map (:[]) ) board
  in
    unlines rows

updateBoard :: [[Char]] -> Int -> Int -> Char -> Int -> Int -> Char -> [[Char]]
updateBoard board rowW colW charW rowB colB charB =
  let
    updatedBoardW = if rowW >= 0 && rowW < 8 && colW >= 0 && colW < 8
                      then updateRow board rowW colW charW
                      else board
    updatedBoardB = if rowB >= 0 && rowB < 8 && colB >= 0 && colB < 8
                      then updateRow updatedBoardW rowB colB charB
                      else updatedBoardW
  in
    updatedBoardB

updateRow :: [[Char]] -> Int -> Int -> Char -> [[Char]]
updateRow board row col char =
  let
    (before, rowToUpdate : after) = splitAt row board
    (beforeCell, _ : afterCell) = splitAt col rowToUpdate
    updatedRow = beforeCell ++ [char] ++ afterCell
  in
    before ++ [updatedRow] ++ after

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (rowA, colA) (rowB, colB) =
  rowA == rowB || colA == colB || abs (rowA - rowB) == abs (colA - colB)
