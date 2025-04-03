module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = processFrames 1 0 rolls
  where
    -- Process frames recursively. If there are no rolls remaining, we consider the game complete.
    processFrames :: Int -> Int -> [Int] -> Either BowlingError Int
    processFrames frame _idx [] = Right 0
    processFrames frame idx rs
      | frame < 10 = do
          (frameScore, newIdx, remaining) <- processFrameNonTenth idx rs
          restScore <- processFrames (frame + 1) newIdx remaining
          return (frameScore + restScore)
      | frame == 10 = do
          (frameScore, _newIdx, _remaining) <- processFrameTenth idx rs
          return frameScore
      | otherwise = Right 0

    -- Process a non-tenth frame.
    processFrameNonTenth :: Int -> [Int] -> Either BowlingError (Int, Int, [Int])
    processFrameNonTenth _idx [] = Left IncompleteGame
    processFrameNonTenth idx (r:rs)
      | r < 0 || r > 10 = Left (InvalidRoll idx r)
      | r == 10 = -- Strike
          case rs of
            (b1:b2:rest) ->
              if b1 < 0 || b1 > 10 then Left (InvalidRoll (idx + 1) b1)
              else if b2 < 0 || b2 > 10 then Left (InvalidRoll (idx + 2) b2)
              else Right (10 + b1 + b2, idx + 1, rs)
            _ -> Left IncompleteGame
      | otherwise = -- Not a strike, so expect two rolls.
          case rs of
            [] -> Left IncompleteGame
            (r2:rest) ->
              if r2 < 0 || r2 > 10 then Left (InvalidRoll (idx + 1) r2)
              else if r + r2 > 10 then Left (InvalidRoll (idx + 1) r2)
              else if r + r2 == 10 then -- Spare
                case rest of
                  (bonus:rest') ->
                    if bonus < 0 || bonus > 10 then Left (InvalidRoll (idx + 2) bonus)
                    else Right (10 + bonus, idx + 2, rest')
                  [] -> Left IncompleteGame
              else Right (r + r2, idx + 2, rest)

    -- Process the tenth frame.
    processFrameTenth :: Int -> [Int] -> Either BowlingError (Int, Int, [Int])
    processFrameTenth idx xs =
      case xs of
        (r:rs)
          | r < 0 || r > 10 -> Left (InvalidRoll idx r)
          | r == 10 -> -- Tenth frame strike: requires two bonus rolls.
              case rs of
                (r2:r3:rest) ->
                  if r2 < 0 || r2 > 10 then Left (InvalidRoll (idx + 1) r2)
                  else if r3 < 0 || r3 > 10 then Left (InvalidRoll (idx + 2) r3)
                  else Right (10 + r2 + r3, idx + 3, rest)
                _ -> Left IncompleteGame
          | otherwise -> -- First roll not a strike.
              case rs of
                [] -> Left IncompleteGame
                (r2:rest) ->
                  if r2 < 0 || r2 > 10 then Left (InvalidRoll (idx + 1) r2)
                  else if r + r2 > 10 then Left (InvalidRoll (idx + 1) r2)
                  else if r + r2 == 10 then -- Spare in tenth requires one bonus roll.
                    case rest of
                      (r3:rest') ->
                        if r3 < 0 || r3 > 10 then Left (InvalidRoll (idx + 2) r3)
                        else Right (r + r2 + r3, idx + 3, rest')
                      [] -> Left IncompleteGame
                  else Right (r + r2, idx + 2, rest)
        [] -> Left IncompleteGame
