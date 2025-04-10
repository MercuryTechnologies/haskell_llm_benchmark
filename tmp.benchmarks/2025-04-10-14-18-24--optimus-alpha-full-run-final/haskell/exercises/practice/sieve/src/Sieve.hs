module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) = p : sieve (filter (not . isMultipleOf p) xs)

    -- Returns True if x is a multiple of p (without using division or mod)
    isMultipleOf :: Integer -> Integer -> Bool
    isMultipleOf p x = isMult p x
      where
        isMult a b
          | b < a     = False
          | b == a    = True
          | otherwise = isMult a (b - a)
