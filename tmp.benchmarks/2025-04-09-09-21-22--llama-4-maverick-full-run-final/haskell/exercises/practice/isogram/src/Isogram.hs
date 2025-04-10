module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)

isIsogram :: String -> Bool
isIsogram str = length letters == length (nub letters)
  where
    letters = [toLower c | c <- str, isLetter c]

nub :: Eq a => [a] -> [a]
nub = nub' []
  where
    nub' _ [] = []
    nub' seen (x:xs)
      | x `elem` seen = nub' seen xs
      | otherwise     = x : nub' (x:seen) xs
