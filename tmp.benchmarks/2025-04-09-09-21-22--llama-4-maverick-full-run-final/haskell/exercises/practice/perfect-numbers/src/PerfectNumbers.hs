module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | otherwise = Just $ classify' (aliquotSum n)
  where
    classify' sum
      | sum == n  = Perfect
      | sum > n   = Abundant
      | otherwise = Deficient

aliquotSum :: Int -> Int
aliquotSum n = sum [i | i <- [1..isqrt n], n `mod` i == 0, i /= n] + 
               sum [n `div` i | i <- [1..isqrt n], n `mod` i == 0, n `div` i /= n, n `div` i /= i]
  where
    isqrt = floor . sqrt . fromIntegral
