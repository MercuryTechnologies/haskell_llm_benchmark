module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = (n * (n + 1) `div` 2) ^ 2
-- Alternatively, using sum:
-- squareOfSum n = sum [1..n] ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6
-- Alternatively, using sum and list comprehension:
-- sumOfSquares n = sum [x^2 | x <- [1..n]]
