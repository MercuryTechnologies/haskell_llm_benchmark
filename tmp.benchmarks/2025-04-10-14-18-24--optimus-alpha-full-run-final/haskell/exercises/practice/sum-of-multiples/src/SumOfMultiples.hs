module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let validFactors = filter (/= 0) factors
      multiples f = takeWhile (< limit) [f, f*2 ..]
      allMultiples = concatMap multiples validFactors
      uniqueMultiples = nub allMultiples
  in sum uniqueMultiples
