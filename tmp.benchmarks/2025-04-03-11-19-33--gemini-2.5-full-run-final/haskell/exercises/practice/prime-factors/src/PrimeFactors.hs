module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where
    factors :: Integer -> Integer -> [Integer]
    factors num divisor
      -- Base case: If the number is 1 or less, we're done factoring.
      | num <= 1 = []
      -- Optimization: If the square of the divisor exceeds the number,
      -- the remaining number must be prime (if it's > 1).
      | divisor * divisor > num = [num]
      -- If the number is divisible by the current divisor:
      | num `mod` divisor == 0 =
          -- Add the divisor to the list and continue factoring the quotient
          -- with the same divisor (to catch repeated factors).
          divisor : factors (num `div` divisor) divisor
      -- If not divisible, try the next divisor.
      | otherwise = factors num (divisor + 1)
