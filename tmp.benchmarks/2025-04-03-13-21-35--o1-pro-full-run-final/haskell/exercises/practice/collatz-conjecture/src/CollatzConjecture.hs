module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (go n 0)
  where
    go 1 steps = steps
    go x steps
      | even x    = go (x `div` 2) (steps + 1)
      | otherwise = go (3 * x + 1) (steps + 1)
