module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    norm s = sort $ map toLower s
    xsNorm = norm xs
    isAnagram candidate =
      let candidateNorm = norm candidate
      in candidateNorm == xsNorm && not (equalCaseInsensitive xs candidate)

    equalCaseInsensitive a b = map toLower a == map toLower b
