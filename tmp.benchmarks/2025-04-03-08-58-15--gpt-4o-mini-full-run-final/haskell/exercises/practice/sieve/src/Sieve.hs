module Sieve (primesUpTo) where

import Prelude hiding (div, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)
