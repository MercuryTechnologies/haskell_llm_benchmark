module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
    filter isAnagram xss
  where
    canonical s = sort (map toLower s)
    target = canonical xs
    isAnagram candidate =
      let c = canonical candidate
      in c == target && (map toLower xs /= map toLower candidate)
