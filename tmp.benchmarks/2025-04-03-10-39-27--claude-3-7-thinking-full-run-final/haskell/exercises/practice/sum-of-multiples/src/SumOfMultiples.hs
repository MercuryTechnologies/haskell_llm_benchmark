module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter isMultiple [1..(limit-1)]
  where
    -- Filter out zero factors and take the absolute value of negative factors
    validFactors = map abs $ filter (/= 0) factors
    
    -- Check if a number is a multiple of any of the factors
    isMultiple n = any (\f -> n `mod` f == 0) validFactors
