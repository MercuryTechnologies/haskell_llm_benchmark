module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  -- First validate that all rolls have valid values
  validateRollValues rolls
  
  -- Then play the game, validating structure as we go
  (_, total) <- playGame rolls 0 0 0
  
  return total

-- Validate individual roll values
validateRollValues :: [Int] -> Either BowlingError ()
validateRollValues rolls = 
  case [(i, r) | (r, i) <- zip rolls [0..], r < 0 || r > 10] of
    (i, r):_ -> Left (InvalidRoll i r)
    [] -> Right ()

-- Play the game, validating as we go
playGame :: [Int] -> Int -> Int -> Int -> Either BowlingError (Int, Int)
playGame rolls idx frameIdx total
  | frameIdx == 10 = 
      -- Game complete
      if null rolls
        then Right (idx, total)
        else Left (InvalidRoll idx (head rolls))  -- Too many rolls
  | null rolls = 
      -- Not enough rolls
      Left IncompleteGame
  | frameIdx == 9 = 
      -- 10th frame (special rules)
      playTenthFrame rolls idx total
  | head rolls == 10 = do
      -- Strike
      if length rolls < 3
        then Left IncompleteGame  -- Not enough rolls for strike bonus
        else playGame (tail rolls) (idx + 1) (frameIdx + 1) (total + 10 + rolls !! 1 + rolls !! 2)
  | length rolls < 2 =
      -- Not enough rolls for this frame
      Left IncompleteGame
  | head rolls + rolls !! 1 > 10 =
      -- Invalid frame total
      Left (InvalidRoll (idx + 1) (rolls !! 1))
  | head rolls + rolls !! 1 == 10 = do
      -- Spare
      if length rolls < 3
        then Left IncompleteGame  -- Not enough rolls for spare bonus
        else playGame (drop 2 rolls) (idx + 2) (frameIdx + 1) (total + 10 + rolls !! 2)
  | otherwise =
      -- Open frame
      playGame (drop 2 rolls) (idx + 2) (frameIdx + 1) (total + head rolls + rolls !! 1)

-- Play 10th frame (special rules)
playTenthFrame :: [Int] -> Int -> Int -> Either BowlingError (Int, Int)
playTenthFrame rolls idx total
  | length rolls < 2 =
      -- Not enough rolls for 10th frame
      Left IncompleteGame
  | head rolls == 10 =
      -- Strike in 10th frame (gets 2 more rolls)
      if length rolls < 3
        then Left IncompleteGame
      else if rolls !! 1 /= 10 && rolls !! 1 + rolls !! 2 > 10
        then Left (InvalidRoll (idx + 2) (rolls !! 2))  -- Invalid second bonus
      else if length rolls > 3
        then Left (InvalidRoll (idx + 3) (rolls !! 3))  -- Too many rolls
      else Right (idx + 3, total + 10 + rolls !! 1 + rolls !! 2)
  | head rolls + rolls !! 1 > 10 =
      -- Invalid frame total
      Left (InvalidRoll (idx + 1) (rolls !! 1))
  | head rolls + rolls !! 1 == 10 =
      -- Spare in 10th frame (gets 1 more roll)
      if length rolls < 3
        then Left IncompleteGame
      else if length rolls > 3
        then Left (InvalidRoll (idx + 3) (rolls !! 3))  -- Too many rolls
      else Right (idx + 3, total + 10 + rolls !! 2)
  | otherwise =
      -- Open frame in 10th (only 2 rolls)
      if length rolls > 2
        then Left (InvalidRoll (idx + 2) (rolls !! 2))  -- Too many rolls
      else Right (idx + 2, total + head rolls + rolls !! 1)
