module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    normalizedTarget = map toLower target
    sortedTarget = sort normalizedTarget

    isAnagram candidate =
      let normalizedCandidate = map toLower candidate
      in normalizedCandidate /= normalizedTarget &&
         sort normalizedCandidate == sortedTarget
