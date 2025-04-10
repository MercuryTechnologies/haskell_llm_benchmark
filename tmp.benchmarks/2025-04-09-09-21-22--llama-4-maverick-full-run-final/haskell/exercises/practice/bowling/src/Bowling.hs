module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls
  | not (isGameComplete rolls) = Left IncompleteGame
  | any isInvalidRoll (zip [0..] rolls) = Left (uncurry InvalidRoll (head (filter isInvalidRoll (zip [0..] rolls))))
  | otherwise = Right (sum (scoreFrames rolls))

isGameComplete :: [Int] -> Bool
isGameComplete rolls = case countStrikes rolls of
  10 -> length rolls == 12
  n  -> length rolls == 2 * (10 - n) + n

countStrikes :: [Int] -> Int
countStrikes = length . takeWhile (==10) . take 10

isInvalidRoll :: (Int, Int) -> Bool
isInvalidRoll (_, roll) = roll < 0 || roll > 10
isInvalidRoll (i, roll) | i > 0 && i `mod` 2 == 1 = roll > 10 - (fst (splitAt i rolls) !! (i-1))
                        | otherwise = False
  where rolls = fst (splitAt (i+1) rolls)

scoreFrames :: [Int] -> [Int]
scoreFrames rolls = scoreFrame rolls

scoreFrame :: [Int] -> [Int]
scoreFrame (x:xs)
  | x == 10 && length xs >= 2 = [10 + head xs + head (drop 1 xs)] ++ scoreFrame xs
  | x + head xs == 10 && length xs >= 1 = [10 + head (drop 1 xs)] ++ scoreFrame (drop 2 xs)
  | otherwise = [x + head xs] ++ scoreFrame (drop 2 xs)
scoreFrame (x:[]) = [x]
scoreFrame [] = []

scoreFrame xs@(x:_) 
  | x == 10 && length xs >= 3 = [x + head xs + head (drop 1 xs)] ++ scoreFrame (drop 1 xs)
  | otherwise = scoreFrame' xs

scoreFrame' :: [Int] -> [Int]
scoreFrame' (x:xs)
  | x == 10 = [10 + nextTwo xs] ++ scoreFrame' xs
  | x + head xs == 10 = [10 + head (drop 1 xs)] ++ scoreFrame' (drop 2 xs)
  | otherwise = [x + head xs] ++ scoreFrame' (drop 2 xs)
scoreFrame' _ = []
