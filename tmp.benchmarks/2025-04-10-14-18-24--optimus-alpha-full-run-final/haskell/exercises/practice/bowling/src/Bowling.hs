module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = score' rolls 0 0
  where
    -- Main scoring function: rolls, frame, rollIndex
    score' :: [Int] -> Int -> Int -> Either BowlingError Int
    score' _ 10 _ = Right 0  -- 10 frames scored, ignore any extra rolls
    score' [] frame rollIdx
      | frame < 10 = Left IncompleteGame
      | otherwise  = Right 0
    score' (r:rs) frame rollIdx
      | r < 0 || r > 10 = Left $ InvalidRoll rollIdx r
      | otherwise =
          if frame == 9
            then scoreTenthFrame (r:rs) rollIdx
            else
              if r == 10
                then do
                  bonus <- getNextTwo rs rollIdx
                  rest <- score' rs (frame + 1) (rollIdx + 1)
                  pure (10 + bonus + rest)
                else
                  case rs of
                    [] -> Left IncompleteGame
                    (r2:rs2)
                      | r2 < 0 || r2 > 10 -> Left $ InvalidRoll (rollIdx + 1) r2
                      | r + r2 > 10 -> Left $ InvalidRoll (rollIdx + 1) r2
                      | r + r2 == 10 -> do
                          bonus <- getNextOne rs2 (rollIdx + 2)
                          rest <- score' rs2 (frame + 1) (rollIdx + 2)
                          pure (10 + bonus + rest)
                      | otherwise -> do
                          rest <- score' rs2 (frame + 1) (rollIdx + 2)
                          pure (r + r2 + rest)

    -- Get next two rolls for strike bonus
    getNextTwo :: [Int] -> Int -> Either BowlingError Int
    getNextTwo (a:b:_) idx
      | a < 0 || a > 10 = Left $ InvalidRoll (idx) a
      | b < 0 || b > 10 = Left $ InvalidRoll (idx + 1) b
      | otherwise = Right (a + b)
    getNextTwo _ _ = Left IncompleteGame

    -- Get next one roll for spare bonus
    getNextOne :: [Int] -> Int -> Either BowlingError Int
    getNextOne (a:_) idx
      | a < 0 || a > 10 = Left $ InvalidRoll idx a
      | otherwise = Right a
    getNextOne _ _ = Left IncompleteGame

    -- Special handling for the 10th frame
    scoreTenthFrame :: [Int] -> Int -> Either BowlingError Int
    scoreTenthFrame (a:b:c:_) idx
      | a < 0 || a > 10 = Left $ InvalidRoll idx a
      | b < 0 || b > 10 = Left $ InvalidRoll (idx + 1) b
      | isStrike a =
          if b > 10 then Left $ InvalidRoll (idx + 1) b
          else if (isStrike b && c > 10) then Left $ InvalidRoll (idx + 2) c
          else if c < 0 || c > 10 then Left $ InvalidRoll (idx + 2) c
          else if not (isStrike b) && b + c > 10 then Left $ InvalidRoll (idx + 2) c
          else Right (a + b + c)
      | a + b > 10 = Left $ InvalidRoll (idx + 1) b
      | a + b == 10 =
          if c < 0 || c > 10 then Left $ InvalidRoll (idx + 2) c
          else Right (a + b + c)
      | otherwise = Right (a + b)
    scoreTenthFrame (a:b:[]) idx
      | a < 0 || a > 10 = Left $ InvalidRoll idx a
      | b < 0 || b > 10 = Left $ InvalidRoll (idx + 1) b
      | a == 10 = Left IncompleteGame
      | a + b == 10 = Left IncompleteGame
      | a + b < 10 = Right (a + b)
      | otherwise = Left $ InvalidRoll (idx + 1) b
    scoreTenthFrame (a:[]) idx
      | a < 0 || a > 10 = Left $ InvalidRoll idx a
      | otherwise = Left IncompleteGame
    scoreTenthFrame [] _ = Left IncompleteGame

    isStrike x = x == 10
