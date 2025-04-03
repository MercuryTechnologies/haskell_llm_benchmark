module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  unlines [unwords [charAt (r,c) | c <- [0..7]] | r <- [0..7]]
  where
    charAt (r,c)
      | Just (r,c) == white = "W"
      | Just (r,c) == black = "B"
      | otherwise           = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
  r1 == r2 ||
  c1 == c2 ||
  abs (r1 - r2) == abs (c1 - c2)
