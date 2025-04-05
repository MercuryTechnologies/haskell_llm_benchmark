module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter (isAnagramOf target) candidates
  where
    isAnagramOf :: String -> String -> Bool
    isAnagramOf x y =
      let lowerX = map toLower x
          lowerY = map toLower y
      in lowerX /= lowerY && sortLetters lowerX == sortLetters lowerY
    
    sortLetters :: String -> String
    sortLetters = sort . map toLower
