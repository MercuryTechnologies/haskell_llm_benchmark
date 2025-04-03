module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls
  | length rolls < 12 = Left IncompleteGame
  | any (< 0) rolls || any (> 10) rolls = Left $ InvalidRoll (length rolls) (head (filter (< 0) rolls ++ filter (> 10) rolls))
  | otherwise = Right (calculateScore rolls)

calculateScore :: [Int] -> Int
calculateScore rolls = sumFrames 0 0
  where
    sumFrames frameIndex totalScore
      | frameIndex >= 10 = totalScore
      | otherwise = let (frameScore, nextIndex) = scoreFrame frameIndex
                    in sumFrames nextIndex (totalScore + frameScore)

    scoreFrame index
      | isStrike index = (10 + nextTwoRolls index, index + 1)
      | isSpare index = (10 + nextRoll index, index + 2)
      | otherwise = (sumRolls index, index + 2)

    isStrike index = rolls !! index == 10

    isSpare index = sumRolls index == 10

    sumRolls index = rolls !! index + rolls !! (index + 1)

    nextRoll index = rolls !! (index + 2)

    nextTwoRolls index = rolls !! (index + 1) + rolls !! (index + 2)
