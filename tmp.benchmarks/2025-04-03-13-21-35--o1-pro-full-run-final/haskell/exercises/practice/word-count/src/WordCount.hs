module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount text =
  let ws = extractWords text
      grouped = group (sort ws)
  in map (\g -> (head g, length g)) grouped

-- Convert to lowercase, replace non (alphanumeric/apostrophe) with spaces,
-- split on whitespace, remove leading/trailing apostrophes
extractWords :: String -> [String]
extractWords =
  filter (not . null)
  . map trimApostrophes
  . words
  . map normalize
  . map toLower

-- Keep letters, digits, and apostrophes; turn everything else into space
normalize :: Char -> Char
normalize c
  | isAlphaNum c || c == '\'' = c
  | otherwise = ' '

-- Remove any leading or trailing apostrophes
trimApostrophes :: String -> String
trimApostrophes = dropWhile (=='\'') . dropWhileEnd (=='\'')
  where
    dropWhileEnd p = reverse . dropWhile p . reverse
