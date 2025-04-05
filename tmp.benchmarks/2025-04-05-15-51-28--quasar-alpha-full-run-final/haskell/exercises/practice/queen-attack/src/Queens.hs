module Queens (boardString, canAttack) where

import Data.Maybe (fromMaybe)

boardSize :: Int
boardSize = 8

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [buildRow r | r <- [0..boardSize - 1]]
  where
    buildRow r = unwords [squareChar (r, c) | c <- [0..boardSize - 1]]
    squareChar pos
      | Just pos == white = "W"
      | Just pos == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
    r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)
