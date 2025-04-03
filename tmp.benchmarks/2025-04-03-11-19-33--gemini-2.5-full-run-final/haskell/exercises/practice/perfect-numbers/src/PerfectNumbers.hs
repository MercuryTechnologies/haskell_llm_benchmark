module PerfectNumbers (classify, Classification(..)) where

import Data.List (nub) -- nub is not strictly needed with the [1..n-1] approach but good practice if factor finding was more complex

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- | Calculate the sum of factors of a number, excluding the number itself.
aliquotSum :: Int -> Int
aliquotSum n = sum [i | i <- [1..n-1], n `mod` i == 0]

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing  -- Classification is only for positive integers
  | otherwise = Just $ case compare (aliquotSum n) n of
                  EQ -> Perfect
                  GT -> Abundant
                  LT -> Deficient
