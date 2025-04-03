module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing  -- Invalid input (non-positive integers)
  | n == 1    = Just 0   -- Base case: already at 1, so 0 steps
  | otherwise = fmap (+ 1) (collatz (nextValue n))
  where
    nextValue m
      | even m    = m `div` 2     -- If n is even, divide by 2
      | otherwise = m * 3 + 1     -- If n is odd, multiply by 3 and add 1
