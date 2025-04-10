module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = [annotateRow r row board | (r, row) <- zip [0..] board]

annotateRow :: Int -> String -> [String] -> String
annotateRow r row board = [annotateCell r c board x | (c, x) <- zip [0..] row]

annotateCell :: Int -> Int -> [String] -> Char -> Char
annotateCell r c board x
  | x == '*' = '*'
  | otherwise = let count = countMines r c board in if count == 0 then ' ' else head (show count)

countMines :: Int -> Int -> [String] -> Int
countMines r c board = sum [1 | (dr, dc) <- directions, isValid (r + dr, c + dc), isMine (r + dr, c + dc) board]
  where
    isValid (r, c) = r >= 0 && r < length board && c >= 0 && c < length (head board)
    isMine (r, c) b = (b !! r) !! c == '*'
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
