module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor 1 _ = []
    factor m f
      | m `mod` f == 0 = f : factor (m `div` f) f
      | otherwise      = factor m (f + 1)
