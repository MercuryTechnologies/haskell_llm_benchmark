module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    targetLower = map toLower target
    sortedTarget = sort targetLower
    isAnagram candidate = candidateLower /= targetLower && sortedCandidate == sortedTarget
      where
        candidateLower = map toLower candidate
        sortedCandidate = sort candidateLower
