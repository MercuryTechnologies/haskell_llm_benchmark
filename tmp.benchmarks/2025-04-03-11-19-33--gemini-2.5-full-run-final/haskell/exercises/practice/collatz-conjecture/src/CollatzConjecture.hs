module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (collatzSteps n 0)
  where
    collatzSteps :: Integer -> Integer -> Integer
    collatzSteps current steps
      | current == 1 = steps
      | even current = collatzSteps (current `div` 2) (steps + 1)
      | odd current  = collatzSteps (3 * current + 1) (steps + 1)
