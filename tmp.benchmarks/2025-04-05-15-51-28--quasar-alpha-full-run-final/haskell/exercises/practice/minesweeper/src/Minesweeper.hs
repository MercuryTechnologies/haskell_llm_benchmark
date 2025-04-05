module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.Maybe (catMaybes)

annotate :: [String] -> [String]
annotate board = [ [ annotateCell r c | c <- [0..cols-1] ] | r <- [0..rows-1] ]
  where
    rows = length board
    cols = if null board then 0 else length (head board)

    annotateCell :: Int -> Int -> Char
    annotateCell r c
      | cell == '*' = '*'
      | count == 0  = ' '
      | otherwise   = intToDigit count
      where
        cell = (board !! r) !! c
        count = countMines r c

    countMines :: Int -> Int -> Int
    countMines r c = length $ filter (=='*') $ catMaybes neighbors
      where
        neighbors = [ getCell (r + dr) (c + dc)
                    | dr <- [-1,0,1], dc <- [-1,0,1], (dr,dc) /= (0,0) ]

    getCell :: Int -> Int -> Maybe Char
    getCell r c
      | r < 0 || r >= rows = Nothing
      | c < 0 || c >= cols = Nothing
      | otherwise          = Just ((board !! r) !! c)
