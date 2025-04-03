module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing  -- The conjecture is defined only for positive integers
  | otherwise = Just (stepsToOne n 0)
  where
    stepsToOne :: Integer -> Integer -> Integer
    stepsToOne 1 steps = steps  -- Base case: reached 1
    stepsToOne num steps
      | even num  = stepsToOne (num `div` 2) (steps + 1)        -- If even, divide by 2
      | otherwise = stepsToOne (3 * num + 1) (steps + 1)        -- If odd, multiply by 3 and add 1
