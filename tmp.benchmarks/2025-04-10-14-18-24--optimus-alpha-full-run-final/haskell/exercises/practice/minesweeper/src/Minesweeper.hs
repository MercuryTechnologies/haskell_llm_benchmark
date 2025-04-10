module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.Maybe (catMaybes)

annotate :: [String] -> [String]
annotate board = [ [ annotateCell r c | c <- [0..w-1] ] | r <- [0..h-1] ]
  where
    h = length board
    w = if null board then 0 else length (head board)

    annotateCell :: Int -> Int -> Char
    annotateCell r c
      | getCell r c == '*' = '*'
      | count == 0         = ' '
      | otherwise          = intToDigit count
      where
        count = countMines r c

    getCell :: Int -> Int -> Char
    getCell r c
      | r < 0 || r >= h = ' '
      | c < 0 || c >= w = ' '
      | otherwise       = (board !! r) !! c

    countMines :: Int -> Int -> Int
    countMines r c = length [ () | dr <- [-1..1], dc <- [-1..1], (dr,dc) /= (0,0), getCell (r+dr) (c+dc) == '*' ]
