module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n < 1     = Nothing
    | otherwise = Just (primes !! (n - 1))

primes :: [Integer]
primes = filter isPrime [2..]

isPrime :: Integer -> Bool
isPrime x = null [y | y <- [2..isqrt x], x `mod` y == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral
