module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.List (transpose)

annotate :: [String] -> [String]
annotate board = [ [ annotateCell r c | c <- [0..cols-1] ] | r <- [0..rows-1] ]
  where
    rows = length board
    cols = if null board then 0 else length (head board)
    
    annotateCell :: Int -> Int -> Char
    annotateCell r c
      | board !! r !! c == '*' = '*'
      | otherwise = let count = countMines r c
                    in if count == 0 then ' ' else intToDigit count

    countMines :: Int -> Int -> Int
    countMines r c = length [ () | dr <- [-1..1], dc <- [-1..1], isMine (r + dr) (c + dc) ]

    isMine :: Int -> Int -> Bool
    isMine r c = r >= 0 && r < rows && c >= 0 && c < cols && board !! r !! c == '*'
