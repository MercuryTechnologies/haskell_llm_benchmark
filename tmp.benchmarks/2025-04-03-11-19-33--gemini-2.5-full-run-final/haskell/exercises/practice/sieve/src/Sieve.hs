module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

-- Import necessary modules for mutable arrays within the ST monad
import Data.Array.ST ( STUArray, runSTUArray ) -- Use unboxed array for Bool
import Data.Array.MArray ( newArray, readArray, writeArray, getAssocs )
import Control.Monad ( forM_, when )
import Control.Monad.ST ( ST )
import Data.List (filter) -- Need filter as it might be hidden by default import hiding

-- | Calculate primes up to a given limit using the Sieve of Eratosthenes.
primesUpTo :: Integer -> [Integer]
primesUpTo n
  -- If the limit is less than 2, there are no primes.
  | n < 2     = []
  -- Otherwise, proceed with the sieve algorithm.
  | otherwise =
      -- The result of the ST computation is a list of (index, isPrime) pairs.
      -- We process this list:
      map (toInteger . fst) . -- 3. Convert indices (which are Int) back to Integer.
      filter snd $            -- 2. Keep only pairs where the boolean value (snd) is True.
      runSTUArray action      -- 1. Execute the ST action to get the list of pairs.
  where
    -- The ST action performs the sieve operations on a mutable array.
    -- It returns a list of (index, Bool) pairs representing numbers and their primality.
    action :: ST s [(Int, Bool)]
    action = do
      -- Convert the input Integer 'n' to Int 'n'' for array indexing.
      -- Note: This assumes 'n' fits within the bounds of Int.
      let n' = fromInteger n :: Int
      -- Create a mutable unboxed boolean array, indexed from 2 to n', initialized to True.
      arr <- newArray (2, n') True :: ST s (STUArray s Int Bool)

      -- Calculate the limit for the outer loop: floor(sqrt(n)).
      -- We only need to sieve with primes up to sqrt(n).
      -- Convert n to Double for sqrt function.
      let limit = floor (sqrt (fromInteger n :: Double))

      -- Iterate through numbers p from 2 up to the calculated limit.
      forM_ [2..limit] $ \p -> do
        -- Check if the number p is still marked as prime.
        isPrime <- readArray arr p
        when isPrime $ do
          -- If p is prime, mark all its multiples starting from p*p as not prime (False).
          -- We start from p*p because smaller multiples (like 2*p, 3*p)
          -- would have already been marked by smaller primes (2, 3, ...).
          forM_ [p*p, p*p + p .. n'] $ \m -> do
            writeArray arr m False

      -- After marking all non-primes, retrieve all (index, value) pairs from the array.
      getAssocs arr
