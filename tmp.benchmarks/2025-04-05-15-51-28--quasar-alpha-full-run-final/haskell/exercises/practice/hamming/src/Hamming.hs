module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ countDifferences xs ys
  where
    countDifferences :: String -> String -> Int
    countDifferences [] [] = 0
    countDifferences (a:as) (b:bs)
      | a /= b    = 1 + countDifferences as bs
      | otherwise = countDifferences as bs
