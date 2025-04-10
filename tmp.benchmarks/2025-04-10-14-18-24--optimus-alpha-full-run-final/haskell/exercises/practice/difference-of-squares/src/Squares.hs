module Squares (difference, squareOfSum, sumOfSquares) where

-- | Calculates the difference between the square of the sum and the sum of the squares
--   of the first n natural numbers.
difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

-- | Calculates the square of the sum of the first n natural numbers.
squareOfSum :: Integral a => a -> a
squareOfSum n = s * s
  where s = n * (n + 1) `div` 2

-- | Calculates the sum of the squares of the first n natural numbers.
sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6
