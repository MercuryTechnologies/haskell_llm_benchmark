module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . removeDuplicates $ concatMap (multiples limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples limit factor
    | factor == 0 = []  -- Return an empty list if the factor is zero
    | otherwise = [factor * n | n <- [1..(limit - 1) `div` factor]]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
