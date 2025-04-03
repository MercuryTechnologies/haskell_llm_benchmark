module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

-- Helper function to check if a character is considered part of a word.
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\''

-- Splits a string into words, using any character that is not alphanumeric or apostrophe as a separator.
splitWords :: String -> [String]
splitWords [] = []
splitWords s =
  let (word, rest) = span isWordChar s
      remaining = dropWhile (not . isWordChar) rest
  in if null word
       then splitWords remaining
       else word : splitWords remaining

-- Trims leading and trailing apostrophes from a word.
trimApostrophes :: String -> String
trimApostrophes = dropWhile (=='\'') . reverse . dropWhile (=='\'') . reverse

wordCount :: String -> [(String, Int)]
wordCount xs =
  let wordsList = map (trimApostrophes . map toLower) (splitWords xs)
      grouped = group (sort wordsList)
  in map (\g -> (head g, length g)) grouped
