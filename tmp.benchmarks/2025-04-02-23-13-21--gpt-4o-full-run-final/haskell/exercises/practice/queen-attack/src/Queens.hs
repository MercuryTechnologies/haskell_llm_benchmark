module Queens (boardString, canAttack) where

import Data.List (intercalate)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = intercalate "\n" [rowString r | r <- [0..7]]
  where
    rowString r = unwords [cellString (r, c) | c <- [0..7]]
    cellString pos
      | Just pos == white = "W"
      | Just pos == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
  r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)
