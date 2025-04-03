module Prime (nth) where

-- | Returns the nth prime number, or Nothing if n is not positive.
nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

-- | An infinite list of prime numbers.
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

-- | Checks if a number is prime.
-- Only odd numbers are checked, and we only check divisibility
-- by odd numbers up to the square root of n.
isPrime :: Integer -> Bool
isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile (\x -> x * x <= n) [3,5..]
