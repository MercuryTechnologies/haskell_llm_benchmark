module Sieve (primesUpTo) where

import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) = p : sieve (filter (notMultiple p) xs)

    notMultiple p x  = not (multipleOf p x)
    multipleOf p x   = go p x p
      where
        go base target step
          | step == target = True
          | step > target  = False
          | otherwise      = go base target (step + base)
