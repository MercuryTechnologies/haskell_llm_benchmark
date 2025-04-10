module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.Array

primesUpTo :: Integer -> [Integer]
primesUpTo n = [i | (i, True) <- assocs $ sieve n]

sieve :: Integer -> Array Integer Bool
sieve n = accumArray (flip ($)) True (2, n) $ concatMap mark [2..isqrt n]
  where
    isqrt :: Integer -> Integer
    isqrt = floor . sqrt . fromInteger

    mark :: Integer -> [(Integer, Bool -> Bool)]
    mark i = [(j, const False) | j <- [i*i, i*i+i..n]]
