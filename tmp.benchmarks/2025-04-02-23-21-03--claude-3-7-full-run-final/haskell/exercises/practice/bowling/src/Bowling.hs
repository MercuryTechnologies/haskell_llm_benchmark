module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = validateRolls rolls >>= scoreGame

validateRolls :: [Int] -> Either BowlingError [Int]
validateRolls rolls = validateRolls' rolls 0
  where
    validateRolls' [] _ = Right rolls
    validateRolls' (r:rs) idx
      | r < 0 || r > 10 = Left $ InvalidRoll idx r
      | otherwise = validateRolls' rs (idx + 1)

scoreGame :: [Int] -> Either BowlingError Int
scoreGame rolls = scoreFrames rolls 1 0
  where
    scoreFrames :: [Int] -> Int -> Int -> Either BowlingError Int
    -- All frames scored
    scoreFrames [] 11 total = Right total
    -- Incomplete game
    scoreFrames [] _ _ = Left IncompleteGame
    -- 10th frame special case
    scoreFrames rs 10 total
      | length rs < 2 = Left IncompleteGame
      | head rs == 10 = -- Strike in 10th frame
          if length rs < 3
            then Left IncompleteGame
            else Right (total + 10 + rs !! 1 + rs !! 2)
      | head rs + rs !! 1 == 10 = -- Spare in 10th frame
          if length rs < 3
            then Left IncompleteGame
            else Right (total + 10 + rs !! 2)
      | otherwise = -- Open frame
          if length rs < 2
            then Left IncompleteGame
            else Right (total + head rs + rs !! 1)
    
    -- Strike
    scoreFrames (10:rs) frame total
      | length rs < 2 = Left IncompleteGame
      | otherwise = scoreFrames rs (frame + 1) (total + 10 + rs !! 0 + rs !! 1)
    
    -- Not enough rolls for a frame
    scoreFrames [_] _ _ = Left IncompleteGame
    
    -- Spare
    scoreFrames (r1:r2:rs) frame total
      | r1 + r2 > 10 = Left $ InvalidRoll 1 r2
      | r1 + r2 == 10 =
          if null rs
            then Left IncompleteGame
            else scoreFrames rs (frame + 1) (total + 10 + head rs)
      | otherwise = scoreFrames rs (frame + 1) (total + r1 + r2)
