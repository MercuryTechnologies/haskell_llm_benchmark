module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime k
  | k < 2     = False
  | otherwise = null [ x | x <- [2..isqrt k], k `mod` x == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
