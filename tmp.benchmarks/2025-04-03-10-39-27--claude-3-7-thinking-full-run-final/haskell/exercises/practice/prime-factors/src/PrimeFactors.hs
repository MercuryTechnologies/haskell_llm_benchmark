module PrimeFactors (primeFactors) where

-- | Compute the prime factors of a given natural number.
-- Returns the list of prime factors in ascending order.
primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n 2
  where
    -- Helper function that keeps track of the current factor being tested
    primeFactors' 1 _ = []  -- Base case: 1 has no prime factors
    primeFactors' m f
      | m < 2         = []  -- Handle negative numbers or zero
      | m `mod` f == 0 = f : primeFactors' (m `div` f) f  -- f is a factor, include it and continue
      | f * f > m      = [m]  -- m is prime if we've checked all potential factors up to sqrt(m)
      | otherwise      = primeFactors' m (f + 1)  -- Try next potential factor
