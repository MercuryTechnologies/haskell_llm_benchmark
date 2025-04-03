module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing  -- Only positive integers are classified
  | otherwise = Just $ case compare (aliquotSum n) n of
      LT -> Deficient
      EQ -> Perfect
      GT -> Abundant

-- Calculate the aliquot sum (sum of all proper divisors)
aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..(n-1)], n `mod` x == 0]
