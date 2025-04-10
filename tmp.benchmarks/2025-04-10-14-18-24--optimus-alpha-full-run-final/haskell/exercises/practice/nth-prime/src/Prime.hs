module Prime (nth) where

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [3,5..(floor (sqrt (fromIntegral n)))]

-- Infinite list of primes
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

-- Return the nth prime (1-based index)
nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))
