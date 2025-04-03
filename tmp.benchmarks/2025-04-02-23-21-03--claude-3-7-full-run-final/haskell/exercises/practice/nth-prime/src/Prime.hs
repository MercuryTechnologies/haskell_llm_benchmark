module Prime (nth) where

-- | Returns the nth prime number (1-indexed)
-- Returns Nothing if n <= 0
nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))
  where
    primes = filter isPrime [2..]

-- | Checks if a number is prime
isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\x -> n `mod` x /= 0) [3, 5 .. upperBound]
  where
    upperBound = floor (sqrt (fromIntegral n))
