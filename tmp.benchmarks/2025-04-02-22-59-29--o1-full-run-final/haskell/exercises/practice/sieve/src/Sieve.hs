module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2 .. n]

sieve :: [Integer] -> [Integer]
sieve []     = []
sieve (p:xs) = p : sieve (crossOut p xs)

crossOut :: Integer -> [Integer] -> [Integer]
crossOut _ []     = []
crossOut p (x:xs)
  | isMultiple x p = crossOut p xs
  | otherwise      = x : crossOut p xs

isMultiple :: Integer -> Integer -> Bool
isMultiple x base
  | x < base  = False
  | otherwise = go 0
  where
    go total
      | total == x = True
      | total  > x = False
      | otherwise  = go (total + base)
