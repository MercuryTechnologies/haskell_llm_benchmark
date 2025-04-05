module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = sum [x | x <- [1..limit-1], any ((==0) . mod x) factors]
