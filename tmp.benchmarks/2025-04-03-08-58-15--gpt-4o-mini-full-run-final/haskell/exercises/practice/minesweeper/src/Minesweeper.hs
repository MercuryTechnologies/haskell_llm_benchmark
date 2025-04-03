module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = [map (uncurry (countMines y)) (zip [0..] row) | (y, row) <- zip [0..] board]
  where
    countMines :: Int -> Int -> Char -> Char
    countMines y x ' ' = let count = adjacentMines y x
                         in if count == 0 then ' ' else head (show count)
    countMines _ _ c = c

    adjacentMines :: Int -> Int -> Int
    adjacentMines y x = length [() | dy <- [-1..1], dx <- [-1..1], 
                                      let ny = y + dy, let nx = x + dx,
                                      (dy /= 0 || dx /= 0), 
                                      ny >= 0, ny < length board, 
                                      nx >= 0, nx < length (board !! ny), 
                                      (board !! ny !! nx) == '*']
