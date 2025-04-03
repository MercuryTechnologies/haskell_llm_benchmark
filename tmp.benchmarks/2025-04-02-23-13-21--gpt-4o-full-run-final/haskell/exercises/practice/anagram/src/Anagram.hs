module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    lowerTarget = map toLower target
    sortedTarget = sort lowerTarget
    isAnagram candidate =
      let lowerCandidate = map toLower candidate
      in lowerCandidate /= lowerTarget && sort lowerCandidate == sortedTarget
