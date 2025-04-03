module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ concatMap multiples factors
  where
    multiples f | f > 0     = takeWhile (< limit) [f, f*2 ..]
                | otherwise = []
