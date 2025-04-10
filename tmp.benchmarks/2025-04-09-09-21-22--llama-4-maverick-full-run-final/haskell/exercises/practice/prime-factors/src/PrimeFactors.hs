module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n 2

primeFactors' :: Integer -> Integer -> [Integer]
primeFactors' 1 _ = []
primeFactors' n divisor
  | n `mod` divisor == 0 = divisor : primeFactors' (n `div` divisor) divisor
  | otherwise = primeFactors' n (divisor + 1)
