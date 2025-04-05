module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls
  | any (\x -> x < 0 || x > 10) rolls = Left $ InvalidRoll (findInvalidIndex rolls) (head $ filter (\x -> x < 0 || x > 10) rolls)
  | not $ isValidGame rolls = Left IncompleteGame
  | otherwise = Right $ calculateScore rolls 0
  where
    findInvalidIndex xs = length $ takeWhile (\x -> x >= 0 && x <= 10) xs

    isValidGame rs = 
      let frames = buildFrames rs
      in length frames == 10 && all isValidFrame frames

    isValidFrame (x:y:[]) = x + y <= 10 || y == 10  -- Normal frame or spare
    isValidFrame [x] = True  -- Strike in last frame
    isValidFrame (x:y:z:[]) = True  -- 10th frame with bonus
    isValidFrame _ = False

    buildFrames [] = []
    buildFrames (x:xs)
      | x == 10 = [x] : buildFrames xs  -- Strike
      | otherwise = 
          case xs of
            (y:ys) -> (x:y:[]) : buildFrames ys  -- Normal frame or spare
            [] -> [[x]]

    calculateScore [] _ = 0
    calculateScore (x:xs) frameCount
      | frameCount == 10 = sum (x:xs)
      | x == 10 =  -- Strike
          10 + sum (take 2 xs) + calculateScore xs (frameCount + 1)
      | otherwise =
          case xs of
            (y:ys) ->
              let frameScore = x + y
                  bonus = if frameScore == 10 then head ys else 0
                  total = frameScore + bonus
              in total + calculateScore ys (frameCount + 1)
            [] -> 0
