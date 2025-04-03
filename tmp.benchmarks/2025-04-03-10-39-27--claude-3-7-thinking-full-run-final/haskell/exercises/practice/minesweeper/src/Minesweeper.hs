module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate [] = []
annotate board = 
    [ map (cellValue r) [0..length (board !! r) - 1] 
    | r <- [0..length board - 1] 
    ]
  where
    cellValue :: Int -> Int -> Char
    cellValue r c
      | (board !! r) !! c == '*' = '*'  -- Keep mines as they are
      | mineCount == 0 = ' '            -- Keep empty if no adjacent mines
      | otherwise = head (show mineCount) -- Replace with count
      where
        mineCount = countAdjacentMines r c
    
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines r c = length
      [ () 
      | dr <- [-1, 0, 1]
      , dc <- [-1, 0, 1]
      , not (dr == 0 && dc == 0)        -- Skip the cell itself
      , let newR = r + dr
      , let newC = c + dc
      , newR >= 0 && newR < length board -- Check row bounds
      , newC >= 0 && newC < length (board !! newR) -- Check column bounds
      , (board !! newR) !! newC == '*'  -- Count only mines
      ]
