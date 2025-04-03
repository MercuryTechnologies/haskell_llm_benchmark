module Prime (nth) where

isPrime :: Integer -> Bool
isPrime x
  | x < 2     = False
  | otherwise = not (any (\d -> x `mod` d == 0) [2 .. floor (sqrt (fromIntegral x))])

primes :: [Integer]
primes = filter isPrime [2..]

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))
