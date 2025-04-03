module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board =
  let
    rows = length board
    cols = if rows > 0 then length (head board) else 0

    getSquare :: Int -> Int -> Char
    getSquare row col
      | row < 0 || row >= rows || col < 0 || col >= cols = ' '  -- Treat out-of-bounds as empty
      | otherwise = (board !! row) !! col

    countMines :: Int -> Int -> Int
    countMines row col =
      sum $ map (\(r, c) -> if getSquare r c == '*' then 1 else 0) $
      [ (row-1, col-1), (row-1, col), (row-1, col+1)
      , (row, col-1),                 (row, col+1)
      , (row+1, col-1), (row+1, col), (row+1, col+1)
      ]

    annotateSquare :: Int -> Int -> Char
    annotateSquare row col
      | getSquare row col == '*' = '*'
      | otherwise =
          let count = countMines row col
          in if count == 0 then ' ' else head (show count)

    annotateRow :: Int -> String
    annotateRow row = [annotateSquare row col | col <- [0..(cols-1)]]

  in
    [annotateRow row | row <- [0..(rows-1)]]
