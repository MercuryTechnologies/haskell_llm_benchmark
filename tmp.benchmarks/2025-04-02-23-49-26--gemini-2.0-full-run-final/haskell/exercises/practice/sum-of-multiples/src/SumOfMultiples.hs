module SumOfMultiples (sumOfMultiples) where

import Data.Set (fromList, toList)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ toList $ fromList multiples
  where
    multiples = [multiple | factor <- factors, factor > 0, multiple <- takeWhile (< limit) $ map (* factor) [1..]]
