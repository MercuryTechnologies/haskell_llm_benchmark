module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = go n 2
  where
    go 1 _ = []
    go x i
      | i * i > x  = [x]
      | x `mod` i == 0 = i : go (x `div` i) i
      | otherwise     = go x (i + 1)
