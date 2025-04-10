module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor 1 _ = []
    factor m d
      | m `mod` d == 0 = d : factor (m `div` d) d
      | d * d > m      = [m] -- m is prime
      | otherwise      = factor m (d + 1)
