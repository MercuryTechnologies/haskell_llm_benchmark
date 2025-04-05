module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let multiplesForFactor f = [x | x <- [1..limit - 1], f /= 0, x `mod` f == 0]
      allMultiples = concatMap multiplesForFactor factors
      uniqueMultiples = nub allMultiples
  in sum uniqueMultiples
