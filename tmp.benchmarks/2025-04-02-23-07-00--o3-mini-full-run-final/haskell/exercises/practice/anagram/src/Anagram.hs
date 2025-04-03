module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates =
  filter isAnagram candidates
  where
    lowerWord = map toLower word
    sortedWord = sort lowerWord
    isAnagram candidate =
      let candidateLower = map toLower candidate
      in candidateLower /= lowerWord && sort candidateLower == sortedWord
