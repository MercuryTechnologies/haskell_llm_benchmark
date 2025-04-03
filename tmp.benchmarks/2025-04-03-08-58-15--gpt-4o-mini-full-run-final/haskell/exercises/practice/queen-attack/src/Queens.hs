module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = unlines [replicate 8 '_' | _ <- [0..7]]
boardString (Just (wr, wc)) (Just (br, bc)) = unlines [row r | r <- [0..7]]
  where
    row r = [case (r, c) of
                (wr, wc) -> 'W'
                (br, bc) -> 'B'
                _        -> '_' | c <- [0..7]]
boardString _ _ = unlines [replicate 8 '_' | _ <- [0..7]]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) = r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)
