module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factor n 2
  where
    factor num divisor
      | num == 1 = []
      | num `mod` divisor == 0 = divisor : factor (num `div` divisor) divisor
      | otherwise = factor num (divisor + 1)
