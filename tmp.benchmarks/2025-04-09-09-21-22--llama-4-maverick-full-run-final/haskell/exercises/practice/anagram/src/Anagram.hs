module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    normalizedXs = sort (map toLower xs)
    isAnagram ys = (sort (map toLower ys) == normalizedXs) && (map toLower ys /= map toLower xs)
