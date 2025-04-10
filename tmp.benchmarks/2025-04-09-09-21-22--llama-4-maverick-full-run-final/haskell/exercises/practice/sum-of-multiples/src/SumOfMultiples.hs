module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as S

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = S.sum $ S.fromList $ concatMap (\f -> [f, f*2 .. limit-1]) factors
