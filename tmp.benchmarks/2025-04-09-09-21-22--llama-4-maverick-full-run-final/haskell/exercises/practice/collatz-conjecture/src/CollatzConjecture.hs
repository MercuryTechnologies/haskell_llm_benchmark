module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n 
  | n <= 0    = Nothing
  | otherwise = Just (collatzSteps n)
  where
    collatzSteps :: Integer -> Integer
    collatzSteps 1 = 0
    collatzSteps n 
      | even n    = 1 + collatzSteps (n `div` 2)
      | otherwise = 1 + collatzSteps (3 * n + 1)
