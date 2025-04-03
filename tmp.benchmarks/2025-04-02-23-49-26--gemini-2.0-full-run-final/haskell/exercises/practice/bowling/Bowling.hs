module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls =
  if length rolls < 12 && not (isCompleteGame rolls)
    then Left IncompleteGame
    else case validateRolls rolls of
           Left err -> Left err
           Right validRolls -> Right $ calculateScore validRolls

validateRolls :: [Int] -> Either BowlingError [Int]
validateRolls rolls =
  foldM validateRoll [] (zip [0..] rolls)
  where
    validateRoll acc (index, roll)
      | roll < 0 || roll > 10 = Left (InvalidRoll index roll)
      | otherwise = Right (acc ++ [roll])

    foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
    foldM f start list = foldl (\m x -> m >>= \y -> f y x) (return start) list

isCompleteGame :: [Int] -> Bool
isCompleteGame rolls =
  let frames = toFrames rolls
  in case frames of
       Right fs -> length fs == 10
       Left _ -> False

calculateScore :: [Int] -> Int
calculateScore rolls = calculateScore' (toFrames rolls) 0
  where
    calculateScore' :: Either BowlingError [[Int]] -> Int -> Int
    calculateScore' (Left _) total = total  -- Should not happen, validation already done
    calculateScore' (Right frames) total = sum (map calculateFrameScore frames)

    calculateFrameScore :: [Int] -> Int
    calculateFrameScore frame =
      case frame of
        [10] -> 10 + strikeBonus frame rolls  -- Strike
        [a, b] | a + b == 10 -> 10 + spareBonus frame rolls  -- Spare
        [a, b] -> a + b  -- Open frame
        _ -> 0 -- Should not happen, toFrames should ensure valid frames

    strikeBonus :: [Int] -> [Int] -> Int
    strikeBonus frame allRolls =
      let frameIndex = findFrameIndex frame allRolls
          rollsAfterStrike = drop (frameIndex + 1) allRolls
      in case rollsAfterStrike of
           (a:b:_) -> a + b
           (a:[]) -> a
           [] -> 0

    spareBonus :: [Int] -> [Int] -> Int
    spareBonus frame allRolls =
      let frameIndex = findFrameIndex frame allRolls
          rollsAfterSpare = drop (frameIndex + 2) allRolls
      in case rollsAfterSpare of
           (a:_) -> a
           [] -> 0

    findFrameIndex :: [Int] -> [Int] -> Int
    findFrameIndex frame allRolls =
      case findFrameIndex' frame allRolls 0 of
        Just index -> index
        Nothing -> -1

    findFrameIndex' :: [Int] -> [Int] -> Int -> Maybe Int
    findFrameIndex' _ [] _ = Nothing
    findFrameIndex' frame rolls index
      | take (length frame) rolls == frame = Just index
      | otherwise = findFrameIndex' frame (drop 1 rolls) (index + 1)

toFrames :: [Int] -> Either BowlingError [[Int]]
toFrames rolls = toFrames' rolls [] 1
  where
    toFrames' :: [Int] -> [[Int]] -> Int -> Either BowlingError [[Int]]
    toFrames' [] acc _ | length acc == 10 = Right acc
                        | otherwise = Right acc
    toFrames' (roll:rest) acc frameNum
      | frameNum == 10 = Right (acc ++ [roll:rest])
      | roll == 10 = toFrames' rest (acc ++ [[roll]]) (frameNum + 1)
      | otherwise =
          case rest of
            (roll2:rest2) | (roll + roll2) >= 10 && frameNum < 10 && (roll + roll2) /= 10 -> Left IncompleteGame
                           | (roll + roll2) < 0 -> Left (InvalidRoll (length acc + 1) (roll + roll2))
                           | otherwise -> toFrames' rest2 (acc ++ [[roll, roll2]]) (frameNum + 1)
            [] -> Left IncompleteGame
