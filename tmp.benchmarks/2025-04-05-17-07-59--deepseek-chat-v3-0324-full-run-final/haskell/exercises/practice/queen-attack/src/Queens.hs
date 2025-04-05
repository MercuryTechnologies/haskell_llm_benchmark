module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map (concat . map square) rows
  where
    rows = [[(r, c) | c <- [0..7]] | r <- [0..7]]
    square pos = case (white, black) of
      (Just w, Just b) | pos == w && pos == b -> "X "
      (Just w, _) | pos == w -> "W "
      (_, Just b) | pos == b -> "B "
      _ -> "_ "

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2)
  | x1 == x2 = True  -- same row
  | y1 == y2 = True  -- same column
  | abs (x1 - x2) == abs (y1 - y2) = True  -- same diagonal
  | otherwise = False
