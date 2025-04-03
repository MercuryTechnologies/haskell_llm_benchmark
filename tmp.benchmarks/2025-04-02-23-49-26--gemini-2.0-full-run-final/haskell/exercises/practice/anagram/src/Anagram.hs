module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter (isAnagram target) candidates

isAnagram :: String -> String -> Bool
isAnagram target candidate =
  let normalizedTarget = sort $ map toLower target
      normalizedCandidate = sort $ map toLower candidate
  in normalizedTarget == normalizedCandidate && map toLower target /= map toLower candidate
