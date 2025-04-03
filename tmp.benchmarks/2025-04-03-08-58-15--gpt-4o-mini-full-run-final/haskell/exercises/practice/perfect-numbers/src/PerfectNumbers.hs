module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | aliquotSum == n = Just Perfect
    | aliquotSum > n  = Just Abundant
    | otherwise       = Just Deficient
  where
    aliquotSum = sum [x | x <- [1..(n `div` 2)], n `mod` x == 0]
