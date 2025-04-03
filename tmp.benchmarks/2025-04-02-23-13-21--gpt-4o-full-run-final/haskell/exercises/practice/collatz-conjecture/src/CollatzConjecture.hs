module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (collatzSteps n 0)
  where
    collatzSteps 1 steps = steps
    collatzSteps x steps
      | even x    = collatzSteps (x `div` 2) (steps + 1)
      | otherwise = collatzSteps (3 * x + 1) (steps + 1)
