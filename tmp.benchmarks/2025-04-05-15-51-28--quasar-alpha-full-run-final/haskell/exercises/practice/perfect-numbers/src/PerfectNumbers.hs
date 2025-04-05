module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | aliquot < n  = Just Deficient
  | aliquot == n = Just Perfect
  | aliquot > n  = Just Abundant
  where
    aliquot = sum (factors n)

-- Helper function to get proper divisors of n (excluding n itself)
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n `div` 2], n `mod` x == 0]
