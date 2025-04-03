module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise =
      let sumFactors = sum [ x | x <- [1 .. (n `div` 2)], n `mod` x == 0 ]
      in Just $ if sumFactors == n
                then Perfect
                else if sumFactors > n
                     then Abundant
                     else Deficient
