module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . Set.toList $ Set.fromList $ concatMap (`multiplesBelow` limit) factors
  where
    multiplesBelow factor lim = [x | x <- [factor, factor*2 .. lim-1]]
