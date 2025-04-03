module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

-- | Normalizes a word for anagram comparison: converts to lowercase and sorts characters.
normalize :: String -> String
normalize = sort . map toLower

-- | Checks if two strings are anagrams of each other, ignoring case.
--   Also ensures the words are not identical (case-insensitively).
isAnagram :: String -> String -> Bool
isAnagram word1 word2 =
  let lowerWord1 = map toLower word1
      lowerWord2 = map toLower word2
  in length word1 == length word2 &&         -- Check length first for efficiency
     lowerWord1 /= lowerWord2 &&             -- Words must not be identical (case-insensitive)
     normalize word1 == normalize word2      -- Check if sorted lowercase versions are equal

-- | Given a target word and a list of candidate words, find the subset of
--   candidates that are anagrams of the target.
anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss
