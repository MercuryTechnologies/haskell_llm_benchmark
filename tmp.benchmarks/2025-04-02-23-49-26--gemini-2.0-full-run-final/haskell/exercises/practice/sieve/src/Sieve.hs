module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (notMultiple x) xs)

notMultiple :: Integer -> Integer -> Bool
notMultiple prime num = num `rem` prime /= 0
