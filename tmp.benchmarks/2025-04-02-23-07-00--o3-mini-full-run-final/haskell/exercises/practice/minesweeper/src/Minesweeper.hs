module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = [ [ processCell i j | j <- [0..width - 1] ] | i <- [0..height - 1]]
  where
    height = length board
    width = if height == 0 then 0 else length (head board)

    processCell i j =
      case board !! i !! j of
        '*' -> '*'
        _   -> let count = adjacentMines i j
               in if count == 0 then ' ' else intToDigit count

    adjacentMines i j = length [ () | di <- [-1..1], dj <- [-1..1],
                                  (di, dj) /= (0,0),
                                  let ni = i + di, let nj = j + dj,
                                  ni >= 0, nj >= 0, ni < height, nj < width,
                                  board !! ni !! nj == '*']
