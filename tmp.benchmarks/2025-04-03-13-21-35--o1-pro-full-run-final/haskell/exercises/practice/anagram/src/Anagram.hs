module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates =
    filter isAnagram candidates
  where
    normalize str = sort (map toLower str)
    targetNormalized = normalize target
    targetLower = map toLower target

    isAnagram candidate =
      let candidateNormalized = normalize candidate
          candidateLower = map toLower candidate
      in candidateNormalized == targetNormalized
         && candidateLower /= targetLower
