module Prime (nth) where

-- Helper function to check if a number is prime
isPrime :: Integer -> Bool
isPrime k
  | k <= 1    = False -- Numbers less than or equal to 1 are not prime
  | k == 2    = True  -- 2 is the first prime
  | even k    = False -- Even numbers greater than 2 are not prime
  | otherwise = all (\x -> k `mod` x /= 0) [3, 5..limit] -- Check divisibility only by odd numbers up to sqrt(k)
  where
    -- Calculate the integer square root
    limit = floor (sqrt (fromInteger k))

-- Infinite list of prime numbers, generated lazily
primes :: [Integer]
primes = filter isPrime [2..]

-- Function to find the nth prime number
nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing -- There is no 0th or negative prime
  | otherwise = Just (primes !! (n - 1)) -- Access the nth prime (using 0-based index n-1) from the lazy list
