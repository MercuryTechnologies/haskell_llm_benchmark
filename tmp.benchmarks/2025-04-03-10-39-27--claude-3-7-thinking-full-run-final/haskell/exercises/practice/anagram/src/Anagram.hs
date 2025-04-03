module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target = filter (isAnagramOf target)
  where
    isAnagramOf word1 word2 = 
      normalized1 /= normalized2 && 
      sort normalized1 == sort normalized2
      where
        normalized1 = map toLower word1
        normalized2 = map toLower word2
