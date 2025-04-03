module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = let sum = (n * (n + 1)) `div` 2
                 in sum * sum

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum [x * x | x <- [1..n]]
