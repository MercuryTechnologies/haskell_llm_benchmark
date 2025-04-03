module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = runST $ do
      sieve <- newArray (2, n) True :: ST s (STUArray s Integer Bool)
      forM_ [2..floor . sqrt $ fromIntegral n] $ \p -> do
        isPrime <- readArray sieve p
        when isPrime $
          forM_ [p*p, p*p+p..n] $ \k -> writeArray sieve k False
      filterM (readArray sieve) [2..n]
