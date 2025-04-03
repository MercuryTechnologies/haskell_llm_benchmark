module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = calculateScore rolls 0 0
  where
    calculateScore :: [Int] -> Int -> Int -> Either BowlingError Int
    calculateScore [] frame total
      | frame < 10 = Left IncompleteGame
      | otherwise = Right total
    calculateScore (r:rs) frame total
      | r < 0 || r > 10 = Left $ InvalidRoll frame r
      | frame >= 10 = Right total
      | r == 10 = calculateScore rs (frame + 1) (total + 10 + bonusForStrike rs)
      | otherwise = case rs of
          (r2:rs') 
            | r + r2 > 10 -> Left $ InvalidRoll frame r2
            | otherwise -> calculateScore rs' (frame + 1) (total + r + r2 + bonusForSpare r r2 rs')
          _ -> Left IncompleteGame

    bonusForStrike :: [Int] -> Int
    bonusForStrike (r1:r2:_) = r1 + r2
    bonusForStrike _ = 0

    bonusForSpare :: Int -> Int -> [Int] -> Int
    bonusForSpare r1 r2 (r3:_) | r1 + r2 == 10 = r3
    bonusForSpare _ _ _ = 0
