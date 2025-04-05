module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve (markMultiples p xs)
        markMultiples p = filter (\x -> x `rem` p /= 0)
