module Sieve (primesUpTo) where

import Data.Array.UArray (UArray, listArray, (!), (//), assocs)
import Data.List (foldl')

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise =
      let -- Create an array representing numbers from 2 to n, initially all True (potential primes)
          bounds = (2, n)
          -- UArray is generally more efficient for Bool than Array
          initialArray :: UArray Integer Bool
          initialArray = listArray bounds (repeat True)

          -- The sieve only needs to check for primes up to sqrt(n)
          limit = floor . sqrt . fromIntegral $ n

          -- Recursive function to perform the sieving
          sieve :: Integer -> UArray Integer Bool -> UArray Integer Bool
          sieve p arr
            -- Stop when p exceeds the square root limit
            | p > limit = arr
            -- If p is marked as composite (False), skip it and move to the next number
            | not (arr ! p) = sieve (p + 1) arr
            -- If p is marked as prime (True), mark its multiples as composite
            | otherwise =
                let -- Multiples to mark: p*p, p*p + p, p*p + 2p, ... up to n
                    multiples = [m | m <- [p * p, p * p + p .. n]]
                    -- Create updates for the array: set multiples to False
                    updates = [(m, False) | m <- multiples]
                    -- Apply updates to the array
                    updatedArr = arr // updates
                 in sieve (p + 1) updatedArr

          -- Run the sieve starting from p = 2
          finalArray = sieve 2 initialArray

       -- Extract the indices (primes) where the value is still True
       in [i | (i, isPrime) <- assocs finalArray, isPrime]

