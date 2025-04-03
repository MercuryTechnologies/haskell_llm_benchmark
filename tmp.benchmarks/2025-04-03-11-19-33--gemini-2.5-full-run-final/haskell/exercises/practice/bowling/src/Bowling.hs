module Bowling (score, BowlingError(..)) where

import Control.Monad (unless)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- Helper to safely get an element from a list
safeGet :: [a] -> Int -> Maybe a
safeGet xs i | i >= 0 && i < length xs = Just (xs !! i)
             | otherwise               = Nothing

-- Helper to convert Maybe to Either
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

-- Validate individual roll values (0-10)
validateRolls :: Int -> [Int] -> Either BowlingError ()
validateRolls _ [] = Right ()
validateRolls idx (r:rs)
    | r < 0 || r > 10 = Left (InvalidRoll idx r)
    | otherwise       = validateRolls (idx + 1) rs

-- Validate bonus rolls specifically for the 10th frame after a strike
validateTenthFrameBonus :: Int -> Int -> Int -> Either BowlingError ()
validateTenthFrameBonus rollIdx2 bonus1 bonus2
    | bonus1 < 0 || bonus1 > 10 = Left (InvalidRoll rollIdx2 bonus1)
    | bonus2 < 0 || bonus2 > 10 = Left (InvalidRoll (rollIdx2 + 1) bonus2)
    -- If the first bonus roll was not a strike, the sum of the two bonus rolls cannot exceed 10
    | bonus1 /= 10 && bonus1 + bonus2 > 10 = Left (InvalidRoll (rollIdx2 + 1) bonus2)
    | otherwise = Right ()

-- Validate bonus roll specifically for the 10th frame after a spare
validateTenthFrameBonusSpare :: Int -> Int -> Either BowlingError ()
validateTenthFrameBonusSpare rollIdx3 bonus
    | bonus < 0 || bonus > 10 = Left (InvalidRoll rollIdx3 bonus)
    | otherwise = Right ()

-- Recursive function to calculate score frame by frame
calculateScore :: Int -> Int -> [Int] -> Either BowlingError Int
calculateScore frameIdx rollIdx allRolls
    -- Base case: Processed 10 frames. Check if there are unexpected extra rolls.
    | frameIdx == 10 =
        if rollIdx == length allRolls
        then Right 0 -- Game finished correctly, no more rolls
        else -- Game finished, but there are extra rolls. Error on the first extra roll.
             let extraRollValue = allRolls !! rollIdx -- This access is safe because rollIdx < length allRolls
             in Left (InvalidRoll rollIdx extraRollValue)
    | otherwise = do
        -- Try to get the first roll of the frame
        r1 <- maybeToEither IncompleteGame (safeGet allRolls rollIdx)

        if r1 == 10 then do -- Strike
            -- Need two subsequent rolls for bonus calculation
            bonus1 <- maybeToEither IncompleteGame (safeGet allRolls (rollIdx + 1))
            bonus2 <- maybeToEither IncompleteGame (safeGet allRolls (rollIdx + 2))
            let currentFrameScore = 10 + bonus1 + bonus2

            -- Perform special validation if this is the 10th frame
            -- Note: This validation happens *before* the base case check for extra rolls
            _ <- if frameIdx == 9 then validateTenthFrameBonus (rollIdx + 1) bonus1 bonus2 else Right ()

            -- Recursively calculate score for remaining frames
            remainingScore <- calculateScore (frameIdx + 1) (rollIdx + 1) allRolls
            Right (currentFrameScore + remainingScore)

        else do -- Not a strike, need the second roll
            r2 <- maybeToEither IncompleteGame (safeGet allRolls (rollIdx + 1))
            -- Validate that the two rolls in the frame don't exceed 10 pins
            unless (r1 + r2 <= 10) $ Left (InvalidRoll (rollIdx + 1) r2)

            if r1 + r2 == 10 then do -- Spare
                -- Need one subsequent roll for bonus calculation
                bonus <- maybeToEither IncompleteGame (safeGet allRolls (rollIdx + 2))
                let currentFrameScore = 10 + bonus

                -- Perform special validation if this is the 10th frame
                -- Note: This validation happens *before* the base case check for extra rolls
                _ <- if frameIdx == 9 then validateTenthFrameBonusSpare (rollIdx + 2) bonus else Right ()

                -- Recursively calculate score for remaining frames
                remainingScore <- calculateScore (frameIdx + 1) (rollIdx + 2) allRolls
                Right (currentFrameScore + remainingScore)

            else do -- Open frame
                let currentFrameScore = r1 + r2

                -- Removed the check for extra rolls here, as the base case now handles it.
                -- _ <- if frameIdx == 9 && isJust (safeGet allRolls (rollIdx + 2))
                --      then Left IncompleteGame -- This was incorrect, should be InvalidRoll or handled by base case
                --      else Right ()

                -- Recursively calculate score for remaining frames
                remainingScore <- calculateScore (frameIdx + 1) (rollIdx + 2) allRolls
                Right (currentFrameScore + remainingScore)

-- Public score function
score :: [Int] -> Either BowlingError Int
score rolls = do
    -- First, validate all individual roll values
    validateRolls 0 rolls
    -- Then, calculate the score recursively, handling frame logic and game completion/errors
    calculateScore 0 0 rolls
