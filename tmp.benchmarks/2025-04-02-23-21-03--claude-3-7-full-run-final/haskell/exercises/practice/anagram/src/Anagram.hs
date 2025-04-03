module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates = filter isAnagram candidates
  where
    normalizedWord = sort (map toLower word)
    isAnagram candidate = 
      let normalizedCandidate = sort (map toLower candidate)
      in normalizedWord == normalizedCandidate && 
         map toLower word /= map toLower candidate
