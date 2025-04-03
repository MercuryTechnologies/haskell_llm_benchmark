module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, notMultiple x p]
    
    notMultiple x p = x < p * p || x `notDivisibleBy` p
    
    notDivisibleBy x p = go x p
      where
        go x' p'
          | x' < p'   = True
          | x' == p'  = False
          | otherwise = go (x' - p') p'
