module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n
  | n < 2 = []
  | otherwise = go n 2
  where
    go x d
      | x == 1        = []
      | d * d > x     = [x]
      | x `mod` d == 0 = d : go (x `div` d) d
      | otherwise     = go x (d + 1)
