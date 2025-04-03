module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate [] = []
annotate board =
  let height = length board
      width  = length (head board)
  in [ [ annotateCell r c | c <- [0..width-1] ]
     | r <- [0..height-1] ]
  where
    annotateCell r c =
      if board !! r !! c == '*'
        then '*'
        else let count = countMines r c
             in if count == 0 then ' ' else intToDigit count

    countMines r c =
      length
        [ ()
        | dr <- [-1..1]
        , dc <- [-1..1]
        , not (dr == 0 && dc == 0)
        , let nr = r + dr
        , let nc = c + dc
        , nr >= 0, nr < length board
        , nc >= 0, nc < length (head board)
        , board !! nr !! nc == '*'
        ]
