module WordCount (wordCount) where

import Data.Char (isAlpha, isSpace, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = map countWords groupedWords
  where
    -- Normalize the input: convert to lowercase and filter words
    wordsList = filter (not . null) . map (filter (/= '\'')) . words $ map toLower xs
    groupedWords = group . sort $ wordsList
    countWords wordGroup = (head wordGroup, length wordGroup)

    -- Custom words function to split by non-alphabetic characters
    words :: String -> [String]
    words [] = []
    words s = case dropWhile isSpace s of
        "" -> []
        s' -> w : words s''
              where (w, s'') = break isAlpha s'
