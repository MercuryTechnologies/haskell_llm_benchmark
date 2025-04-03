module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black
  | white == black && white /= Nothing = error "Queens cannot share the same space"
  | white == Nothing && black == Nothing = unlines [unwords (replicate 8 "_") | _ <- [1..8]]
  | otherwise = unlines ([header] ++ boardRows ++ [header])
  where
    header = "  a b c d e f g h"
    boardRows = [ formatRow r | r <- [0..7] ]
    formatRow r =
      let rowLabel = show (8 - r)
          cells = unwords [ cell r c | c <- [0..7] ]
      in rowLabel ++ " " ++ cells ++ " " ++ rowLabel
    cell r c
      | white == Just (r, c) = "W"
      | black == Just (r, c) = "B"
      | otherwise            = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
  r1 == r2 ||
  c1 == c2 ||
  abs (r1 - r2) == abs (c1 - c2)
