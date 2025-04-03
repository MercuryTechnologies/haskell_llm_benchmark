module SumOfMultiples (sumOfMultiples) where

import Data.List (subsequences, foldl')
import Prelude hiding (lcm)

-- Define our own lcm, so we don't rely on Prelude's version (in case of name clashes).
lcm :: Integer -> Integer -> Integer
lcm a b = abs (a `div` gcd a b * b)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    -- Use inclusion-exclusion for efficiency, avoiding huge list constructions.
    sumOfInclusionExclusion (filter (/= 0) factors) (limit - 1)
  where
    sumOfInclusionExclusion :: [Integer] -> Integer -> Integer
    sumOfInclusionExclusion fs mx =
      -- For all non-empty subsets, sum or subtract arithmetic series of multiples of their lcm.
      sum $ map sumTerm (tail $ subsequences fs)

    sumTerm :: [Integer] -> Integer
    sumTerm subset =
      let l = foldl' lcm 1 subset
          k = mx `div` l
          total = l * k * (k + 1) `div` 2
          sign = if odd (length subset) then 1 else -1
      in sign * total
