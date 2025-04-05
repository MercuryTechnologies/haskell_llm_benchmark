module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)

wordCount :: String -> [(String, Int)]
wordCount input = 
    let words' = extractWords input
        cleanWords = map (removeQuotes . map toLower) words'
        counts = foldr (\word acc -> incrementCount word acc) [] cleanWords
    in counts

extractWords :: String -> [String]
extractWords = words . map replaceNonWordChars
  where
    replaceNonWordChars c
        | isAlphaNum c || c == '\'' = c
        | otherwise = ' '

removeQuotes :: String -> String
removeQuotes word = filter (/= '\'') word

incrementCount :: String -> [(String, Int)] -> [(String, Int)]
incrementCount word [] = [(word, 1)]
incrementCount word ((w, c):rest)
    | word == w = (w, c + 1) : rest
    | otherwise = (w, c) : incrementCount word rest
