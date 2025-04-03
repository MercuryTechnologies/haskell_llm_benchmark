module Prime (nth) where

-- Function to check if a number is prime
isPrime :: Integer -> Bool
isPrime k
  | k < 2     = False
  | k == 2    = True
  | even k    = False
  | otherwise = null [ x | x <- [3,5..floor . sqrt $ fromIntegral k], k `mod` x == 0]

-- Function to find the nth prime number
nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just (primes !! (n - 1))
  where
    primes = filter isPrime [2..]
