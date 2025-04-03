module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

-- Helper function to check if x is a multiple of p without using division.
isMultiple :: Integer -> Integer -> Bool
isMultiple p x
  | x < p     = False
  | x == p    = True
  | otherwise = isMultiple p (x - p)

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) = p : sieve [x | x <- xs, not (isMultiple p x)]
