module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = map (map annotateCell) indexedBoard
  where
    rows = length board
    cols = if null board then 0 else length (head board)
    indexedBoard = [[(i, j) | j <- [0..cols-1]] | i <- [0..rows-1]]
    
    annotateCell :: (Int, Int) -> Char
    annotateCell (i, j) = 
      case (board !! i) !! j of
        '*' -> '*'
        ' ' -> 
          let count = countAdjacentMines i j
          in if count == 0 then ' ' else intToDigit count
        c -> c
    
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines i j = 
      length [ (di, dj) | di <- [-1..1], 
                          dj <- [-1..1],
                          not (di == 0 && dj == 0),
                          isValid (i + di) (j + dj),
                          (board !! (i + di)) !! (j + dj) == '*' ]
    
    isValid :: Int -> Int -> Bool
    isValid i j = i >= 0 && i < rows && j >= 0 && j < cols
