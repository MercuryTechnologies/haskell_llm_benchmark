module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate [] = []
annotate board = map (processRow board) (zip [0..] board)

processRow :: [String] -> (Int, String) -> String
processRow board (rowIdx, row) = map (processCell board rowIdx) (zip [0..] row)

processCell :: [String] -> Int -> (Int, Char) -> Char
processCell board rowIdx (colIdx, cell)
  | cell == '*' = '*'  -- If it's a mine, leave it as is
  | otherwise = case countAdjacentMines board rowIdx colIdx of
                  0 -> ' '  -- If no adjacent mines, leave it empty
                  n -> head (show n)  -- Otherwise, show the count

countAdjacentMines :: [String] -> Int -> Int -> Int
countAdjacentMines board rowIdx colIdx = 
  length $ filter (== '*') $ adjacentCells board rowIdx colIdx

adjacentCells :: [String] -> Int -> Int -> [Char]
adjacentCells board rowIdx colIdx = 
  [getCell board r c | r <- [rowIdx-1..rowIdx+1], 
                       c <- [colIdx-1..colIdx+1], 
                       not (r == rowIdx && c == colIdx),  -- Exclude the cell itself
                       r >= 0, r < length board,  -- Check row bounds
                       c >= 0, c < length (board !! r)]  -- Check column bounds

getCell :: [String] -> Int -> Int -> Char
getCell board rowIdx colIdx = (board !! rowIdx) !! colIdx
