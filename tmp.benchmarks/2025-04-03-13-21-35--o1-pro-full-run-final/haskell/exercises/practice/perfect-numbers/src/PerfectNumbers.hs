module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | sumFactors < n  = Just Deficient
  | sumFactors == n = Just Perfect
  | otherwise       = Just Abundant
  where
    sumFactors = sum [ x | x <- [1..(n `div` 2)], n `mod` x == 0]
