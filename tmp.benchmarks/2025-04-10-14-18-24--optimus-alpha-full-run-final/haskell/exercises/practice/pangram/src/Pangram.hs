module Pangram (isPangram) where

import Data.Char (isAsciiLower, isAsciiUpper, toLower)

isPangram :: String -> Bool
isPangram text =
  let asciiLetters = [c | c <- map toLower text, isAsciiLower c]
      uniqueLetters = removeDuplicates asciiLetters
  in length uniqueLetters == 26

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise   = x : removeDuplicates xs
