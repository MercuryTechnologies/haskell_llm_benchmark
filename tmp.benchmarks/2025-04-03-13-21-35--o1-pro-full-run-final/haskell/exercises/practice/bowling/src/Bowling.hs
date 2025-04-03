module Bowling (score, BowlingError(..)) where

data BowlingError
    = IncompleteGame
    | InvalidRoll { rollIndex :: Int, rollValue :: Int }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- | Score a complete list of rolls in a single bowling game.
--   Returns either a valid score or an appropriate BowlingError.
-------------------------------------------------------------------------------
score :: [Int] -> Either BowlingError Int
score rolls = do
  -- Label each roll with index for error reporting.
  let indexed = zip [0..] rolls

  -- Parse the first 9 frames.
  (first9, leftover) <- parse9Frames indexed []

  -- Parse the 10th frame from leftover rolls.
  (tenth, leftover2) <- parseTenth leftover

  -- We must not have extra rolls after the 10th frame is parsed:
  case leftover2 of
    [] -> Right (scoreGame (first9 ++ [tenth]))
    (badIdx, badPins) : _ -> Left (InvalidRoll badIdx badPins)


-------------------------------------------------------------------------------
-- Parsing logic
-------------------------------------------------------------------------------
-- | Parse first 9 frames from the list of indexed rolls.
--   Accumulates frames in 'acc'.
--   Returns (listOfFrames, leftover) or an error if there's an invalid/incomplete parse.
-------------------------------------------------------------------------------
parse9Frames :: [(Int, Int)]                -- remaining rolls
             -> [[Int]]                     -- accumulated frames
             -> Either BowlingError ([[Int]], [(Int, Int)])
parse9Frames rolls acc
  | length acc == 9 = Right (reverse acc, rolls)
  | otherwise = do
      (frame, leftover) <- parseNormalFrame rolls
      parse9Frames leftover (frame : acc)

-------------------------------------------------------------------------------
-- | Parse a single 'normal' frame (not the 10th).
--   If the first roll is a strike, frame = [10].
--   Otherwise we need two rolls, sum <= 10.
-------------------------------------------------------------------------------
parseNormalFrame :: [(Int, Int)]
                 -> Either BowlingError ([Int], [(Int, Int)])
parseNormalFrame [] = Left IncompleteGame
parseNormalFrame ((i1, r1) : rest)
  | r1 < 0 || r1 > 10 = Left (InvalidRoll i1 r1)
  | r1 == 10 =
      -- strike frame: [10]
      Right ([10], rest)
  | otherwise =
      -- need a second roll
      case rest of
        [] -> Left IncompleteGame
        ((i2, r2) : rs2)
          | r2 < 0 || r2 > 10 ->
              Left (InvalidRoll i2 r2)
          | r1 + r2 > 10 ->
              Left (InvalidRoll i2 r2)
          | otherwise ->
              Right ([r1, r2], rs2)

-------------------------------------------------------------------------------
-- | Parse the 10th frame, which can have 2 or 3 rolls:
--    1) If first roll is strike => read up to 2 extra.
--    2) Else if first two rolls sum to 10 => read 1 extra.
--    3) Else just those two rolls.
-------------------------------------------------------------------------------
parseTenth :: [(Int, Int)]
           -> Either BowlingError ([Int], [(Int, Int)])
parseTenth [] = Left IncompleteGame
parseTenth ((i1, r1):xs)
  | r1 < 0 || r1 > 10 =
      Left (InvalidRoll i1 r1)
  | r1 == 10 = do
      -- first roll is strike => read two extra
      (r2, leftover1) <- requireOne xs
      (r3, leftover2) <- requireOne leftover1
      if snd r2 < 0 || snd r2 > 10
        then Left (InvalidRoll (fst r2) (snd r2))
        else if snd r3 < 0 || snd r3 > 10
          then Left (InvalidRoll (fst r3) (snd r3))
          else Right ([10, snd r2, snd r3], leftover2)

  | otherwise = do
      -- read second roll
      (r2, leftover1) <- requireOne xs
      let v1 = r1
          v2 = snd r2
          i2 = fst r2
      if v2 < 0 || v2 > 10
        then Left (InvalidRoll i2 v2)
        else if (v1 + v2) == 10
          then do
            -- read bonus ball
            (r3, leftover2) <- requireOne leftover1
            if snd r3 < 0 || snd r3 > 10
              then Left (InvalidRoll (fst r3) (snd r3))
              else Right ([v1, v2, snd r3], leftover2)
          else if (v1 + v2) < 10
            then Right ([v1, v2], leftover1)
            else Left (InvalidRoll i2 v2)

-------------------------------------------------------------------------------
-- | Helper that requires exactly one roll from a list, or fails if empty.
-------------------------------------------------------------------------------
requireOne :: [(Int, Int)]
           -> Either BowlingError ((Int, Int), [(Int, Int)])
requireOne [] = Left IncompleteGame
requireOne (r:rs) = Right (r, rs)

-------------------------------------------------------------------------------
-- Score calculation
-------------------------------------------------------------------------------
scoreGame :: [[Int]] -> Int
scoreGame frames = go frames 0 0
  where
    go _ acc 10 = acc
    go (f:fs) acc frameIndex
      | frameIndex < 9 = case f of
          [10] ->
            let bonus = sum (take 2 (concat fs))
            in go fs (acc + 10 + bonus) (frameIndex + 1)
          [r1, r2]
            | r1 + r2 == 10 ->
              let bonus = headOrZero (concat fs)
              in go fs (acc + 10 + bonus) (frameIndex + 1)
            | otherwise ->
              go fs (acc + r1 + r2) (frameIndex + 1)
          _ ->
            -- shouldn't happen for the first 9 frames if parsing is correct
            go fs acc (frameIndex + 1)
      | otherwise =
          -- 10th frame: just sum up what's there
          acc + sum f

    headOrZero []    = 0
    headOrZero (x:_) = x
