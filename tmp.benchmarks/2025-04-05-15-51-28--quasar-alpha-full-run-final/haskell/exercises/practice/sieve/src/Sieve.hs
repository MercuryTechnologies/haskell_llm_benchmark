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
    sieve (p:xs) = p : sieve (filter (notMultipleOf p) xs)

    notMultipleOf p x = not (isMultiple p x)

    -- Determine if x is a multiple of p without using division or modulo
    isMultiple p x = go p
      where
        go acc
          | acc == x  = True
          | acc > x   = False
          | otherwise = go (acc + p)
