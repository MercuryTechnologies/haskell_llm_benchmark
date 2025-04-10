module Queens (boardString, canAttack) where

import Data.Maybe (fromMaybe)

-- | Render the chess board as a string, given optional positions for white and black queens.
boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [renderRow r | r <- [0..7]]
  where
    renderRow row = unwords [renderSquare row col | col <- [0..7]]
    renderSquare row col
      | Just (row, col) == white = "W"
      | Just (row, col) == black = "B"
      | otherwise                = "_"

-- | Determine if two queens can attack each other.
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
    r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)
