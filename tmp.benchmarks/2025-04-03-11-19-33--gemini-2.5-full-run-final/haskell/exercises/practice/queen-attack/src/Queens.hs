module Queens (boardString, canAttack) where

import Data.List (intercalate)

-- | Generates a string representation of the board with optional queen positions.
--   'W' for white queen, 'B' for black queen, '_' for empty squares.
--   Rows are separated by newlines, columns by spaces.
--   The final string includes a trailing newline.
boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  let rows = [0..7]
      cols = [0..7]
      -- Function to determine the character for a cell (r, c)
      cellChar :: Int -> Int -> Char
      cellChar r c
        | white == Just (r, c) = 'W' -- White queen is at (r, c)
        | black == Just (r, c) = 'B' -- Black queen is at (r, c)
        | otherwise            = '_' -- Square is empty
      -- Function to generate the string for a single row r
      rowString :: Int -> String
      rowString r = intercalate " " [ [cellChar r c] | c <- cols ] -- Create row string with spaces
  -- Generate all row strings, join them with newlines, and add a final newline
  in intercalate "\n" [ rowString r | r <- rows ] ++ "\n"

-- | Determines if two queens at the given positions can attack each other.
--   Queens attack if they are on the same row, column, or diagonal.
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
    r1 == r2 ||                                -- Same row
    c1 == c2 ||                                -- Same column
    abs (r1 - r2) == abs (c1 - c2)             -- Same diagonal
