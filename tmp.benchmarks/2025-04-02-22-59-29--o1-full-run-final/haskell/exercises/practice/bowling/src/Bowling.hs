module Bowling (score, BowlingError(..)) where

data BowlingError
  = IncompleteGame
  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = scoreAll rolls

-- | scoreAll orchestrates the frame-by-frame scoring of a 10-frame bowling game.
scoreAll :: [Int] -> Either BowlingError Int
scoreAll rolls = goFrames 1 0 0
  where
    goFrames :: Int -> Int -> Int -> Either BowlingError Int
    goFrames frame rIndex total
      | frame > 10 =
          -- Once we've processed all 10 frames, return the accumulated total
          Right total
      | frame < 10 = do
          -- Process frames 1..9
          if rIndex >= length rolls
            then Left IncompleteGame
            else do
              firstRoll <- getRoll rIndex
              if firstRoll == 10
                -- Strike (frame is a single roll)
                then do
                  bonus <- getBonus (rIndex + 1) 2
                  goFrames (frame + 1) (rIndex + 1) (total + 10 + bonus)
                else do
                  if (rIndex + 1) >= length rolls
                    then Left IncompleteGame
                    else do
                      secondRoll <- getRoll (rIndex + 1)
                      if firstRoll + secondRoll > 10
                        then Left (InvalidRoll (rIndex + 1) secondRoll)
                        else if firstRoll + secondRoll == 10
                          -- Spare
                          then do
                            bonus <- getBonus (rIndex + 2) 1
                            goFrames (frame + 1) (rIndex + 2) (total + 10 + bonus)
                          else
                            -- Open frame
                            goFrames (frame + 1) (rIndex + 2) (total + firstRoll + secondRoll)
      | otherwise = do
          -- Process the 10th frame (frame == 10)
          if rIndex >= length rolls
            then Left IncompleteGame
            else do
              firstRoll <- getRoll rIndex
              if firstRoll == 10
                then do
                  -- Strike in 10th frame: need two more rolls if available
                  if (rIndex + 1) >= length rolls
                    then Left IncompleteGame
                    else do
                      secondRoll <- getRoll (rIndex + 1)
                      if secondRoll < 0 || secondRoll > 10
                        then Left (InvalidRoll (rIndex + 1) secondRoll)
                        else do
                          if (rIndex + 2) >= length rolls
                            then Left IncompleteGame
                            else do
                              thirdRoll <- getRoll (rIndex + 2)
                              if thirdRoll < 0 || thirdRoll > 10
                                then Left (InvalidRoll (rIndex + 2) thirdRoll)
                                else if secondRoll /= 10 && (secondRoll + thirdRoll) > 10
                                  then Left (InvalidRoll (rIndex + 2) thirdRoll)
                                  else Right (total + firstRoll + secondRoll + thirdRoll)
                else do
                  -- Not a strike for the first roll in 10th frame
                  if (rIndex + 1) >= length rolls
                    then Left IncompleteGame
                    else do
                      secondRoll <- getRoll (rIndex + 1)
                      let frameSum = firstRoll + secondRoll
                      if frameSum > 10
                        then Left (InvalidRoll (rIndex + 1) secondRoll)
                        else if frameSum == 10
                          -- Spare in 10th: we get one extra roll
                          then do
                            if (rIndex + 2) >= length rolls
                              then Left IncompleteGame
                              else do
                                thirdRoll <- getRoll (rIndex + 2)
                                if thirdRoll < 0 || thirdRoll > 10
                                  then Left (InvalidRoll (rIndex + 2) thirdRoll)
                                  else Right (total + firstRoll + secondRoll + thirdRoll)
                          else
                            -- Open frame
                            Right (total + frameSum)

    -- | getRoll fetches one roll from the list, validating range of 0..10
    getRoll :: Int -> Either BowlingError Int
    getRoll i =
      if i >= length rolls
        then Left IncompleteGame
        else
          let val = rolls !! i
           in if val < 0 || val > 10
                then Left (InvalidRoll i val)
                else Right val

    -- | getBonus sums up the next n rolls for strike/spare bonus. If not enough rolls remain, it's incomplete.
    getBonus :: Int -> Int -> Either BowlingError Int
    getBonus start n =
      if (start + n - 1) >= length rolls
        then Left IncompleteGame
        else sum <$> mapM getRoll [start .. start + n - 1]
