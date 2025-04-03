module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n <= 1    = []
  | otherwise = factor : primeFactors (n `div` factor)
  where
    factor = smallestFactor n

-- Find the smallest factor of n
smallestFactor :: Integer -> Integer
smallestFactor n = head [i | i <- [2..n], n `mod` i == 0]
