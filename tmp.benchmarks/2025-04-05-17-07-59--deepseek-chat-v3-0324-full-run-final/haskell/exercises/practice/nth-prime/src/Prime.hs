module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n <= 0    = Nothing
    | otherwise = Just (primes !! (n - 1))
  where
    primes = sieve [2..]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
