module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = factor n 2
  where
    factor 1 _ = []
    factor m divisor
      | m `mod` divisor == 0 = divisor : factor (m `div` divisor) divisor
      | otherwise = factor m (divisor + 1)
