module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [row r | r <- [0..7]]
  where
    row r = unwords [cell r c | c <- [0..7]]
    cell r c
      | Just (r', c') <- white, r == r', c == c' = "W"
      | Just (r', c') <- black, r == r', c == c' = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
  r1 == r2 ||                -- same row
  c1 == c2 ||                -- same column
  abs (r1 - r2) == abs (c1 - c2)  -- same diagonal
