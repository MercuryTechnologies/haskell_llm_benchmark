module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum uniqueMultiples
  where
    -- Generate multiples for each factor
    multiples = [multiples' factor | factor <- factors, factor > 0]
    multiples' factor = [factor, factor * 2 .. limit - 1]
    
    -- Combine all multiples and remove duplicates
    uniqueMultiples = if null factors then [] else foldr union [] multiples
    
    -- Custom union function to merge lists without duplicates
    union [] ys = ys
    union (x:xs) ys
      | x `elem` ys = union xs ys
      | otherwise = x : union xs ys
