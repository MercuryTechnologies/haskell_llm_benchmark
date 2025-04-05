module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = validate rolls 0
  where
    validate :: [Int] -> Int -> Either BowlingError Int
    validate [] _ = computeScore rolls 0 1 0
    validate (r:rs) idx
      | r < 0 || r > 10 = Left $ InvalidRoll idx r
      | otherwise = validate rs (idx + 1)

    computeScore :: [Int] -> Int -> Int -> Int -> Either BowlingError Int
    computeScore _ acc 11 _ = Right acc
    computeScore xs acc frame rollIdx = do
      (frameScore, rest, nextRollIdx) <- frameScoreAndRest xs frame rollIdx
      computeScore rest (acc + frameScore) (frame + 1) nextRollIdx

    frameScoreAndRest :: [Int] -> Int -> Int -> Either BowlingError (Int, [Int], Int)
    frameScoreAndRest [] _ _ = Left IncompleteGame
    frameScoreAndRest (r1:rs) frame rollIdx
      | r1 == 10 = -- strike
          if length rs < 2 then Left IncompleteGame
          else Right (10 + rs !! 0 + rs !! 1, rs, rollIdx + 1)
      | otherwise =
          case rs of
            [] -> Left IncompleteGame
            (r2:rs')
              | r1 + r2 > 10 -> Left $ InvalidRoll (rollIdx + 1) r2
              | r1 + r2 == 10 -> -- spare
                  if null rs' then Left IncompleteGame
                  else Right (10 + head rs', rs', rollIdx + 2)
              | otherwise -> -- open frame
                  Right (r1 + r2, rs', rollIdx + 2)
