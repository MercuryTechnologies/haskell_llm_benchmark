module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just (collatzSteps n 0)
    where
        collatzSteps 1 steps = steps
        collatzSteps n steps
            | even n    = collatzSteps (n `div` 2) (steps + 1)
            | otherwise = collatzSteps (3 * n + 1) (steps + 1)
