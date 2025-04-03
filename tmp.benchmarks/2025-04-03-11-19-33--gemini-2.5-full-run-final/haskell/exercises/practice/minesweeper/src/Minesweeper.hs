module Minesweeper (annotate) where

import Data.Char (intToDigit)
import Data.List (foldl') -- Import foldl' for potentially simpler optimization

annotate :: [String] -> [String]
annotate board
  -- Handle empty board case gracefully
  | null board || null (head board) = []
  | otherwise =
      -- Iterate through each row index r
      [ -- For each row r, iterate through each column index c
        [ annotateSquare r c | c <- [0..numCols-1] ]
      | r <- [0..numRows-1] ]
  where
    -- Get board dimensions
    numRows = length board
    numCols = length (head board)

    -- Check if given coordinates (r, c) are within the board boundaries
    isValid :: Int -> Int -> Bool
    isValid r c = r >= 0 && r < numRows && c >= 0 && c < numCols

    -- Safely get the character at coordinates (r, c)
    -- Returns Nothing if the coordinates are out of bounds
    getCharAt :: Int -> Int -> Maybe Char
    getCharAt r c
      | isValid r c = Just ((board !! r) !! c) -- Access the character if valid
      | otherwise   = Nothing                 -- Return Nothing if out of bounds

    -- Count the number of mines adjacent to the cell at (r, c) using foldl'
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines r c = foldl' countMine 0 deltas
      where
        -- Define the relative offsets for the 8 neighbors
        deltas = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
        -- Accumulator function for the fold
        countMine :: Int -> (Int, Int) -> Int
        countMine acc (dr, dc) =
          let nr = r + dr -- Calculate neighbor row
              nc = c + dc -- Calculate neighbor column
          in case getCharAt nr nc of -- Check the character at the neighbor's position
               Just '*' -> acc + 1 -- If it's a mine, increment count
               _        -> acc     -- Otherwise, keep count the same

    -- Determine the character for the annotated square at (r, c)
    annotateSquare :: Int -> Int -> Char
    annotateSquare r c =
      -- Get the character at the current cell (r, c)
      -- We assume r and c are valid because they come from the main iteration bounds
      let currentSquare = (board !! r) !! c
      in if currentSquare == '*'
           then '*' -- If it's a mine, keep it as a mine
           else -- Otherwise (it's an empty square ' ')
             let count = countAdjacentMines r c -- Calculate adjacent mines
             in if count == 0
                  then ' ' -- If count is 0, keep the square empty
                  else intToDigit count -- Otherwise, convert the count to its character representation
