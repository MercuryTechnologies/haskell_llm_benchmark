module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount = map (\xs -> (head xs, length xs)) . group . sort . words' . map toLower
  where
    -- Split text into words, handling apostrophes in contractions
    words' :: String -> [String]
    words' = reverse . map reverse . go [] []
      where
        go :: String -> [String] -> String -> [String]
        go [] words [] = words
        go current words [] = current : words
        go current words (c:cs)
          | isAlphaNum c = go (c:current) words cs
          | c == '\'' && not (null current) && not (null cs) && isAlphaNum (head cs) = go (c:current) words cs
          | null current = go [] words cs
          | otherwise = go [] (current:words) cs
