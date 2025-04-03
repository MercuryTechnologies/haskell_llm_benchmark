module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []  -- No primes less than 2
  | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `notMultipleOf` p]
    
    -- Check if a number is not a multiple of p without using division
    notMultipleOf x p = go x p
      where
        go num factor
          | num < factor = True
          | num == factor = False
          | otherwise = go (num - factor) factor
