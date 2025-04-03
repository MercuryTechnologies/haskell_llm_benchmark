module WordCount (wordCount) where

import Data.Char (toLower, isLetter, isDigit)
import Data.List (sort, group, dropWhileEnd) -- dropWhileEnd is in base >= 4.8.0

-- Helper function to trim leading/trailing apostrophes from a string.
-- Handles cases like "'quoted'" -> "quoted", "don't" -> "don't".
trimApostrophes :: String -> String
trimApostrophes = dropWhileEnd (== '\'') . dropWhile (== '\'')

-- Helper function to check if a character should be part of a word
-- (letter, digit, or apostrophe).
isWordChar :: Char -> Bool
isWordChar c = isLetter c || isDigit c || c == '\''

-- Replaces non-word characters with spaces to facilitate splitting.
cleanChar :: Char -> Char
cleanChar c
  | isWordChar c = c
  | otherwise    = ' '

wordCount :: String -> [(String, Int)]
wordCount xs = wordCounts
  where
    -- 1. Convert to lowercase
    loweredText = map toLower xs

    -- 2. Replace non-word characters (except apostrophes) with spaces
    cleanedText = map cleanChar loweredText

    -- 3. Split into potential words based on spaces
    potentialWords = words cleanedText

    -- 4. Trim leading/trailing apostrophes from each potential word
    trimmedWords = map trimApostrophes potentialWords

    -- 5. Filter out any empty strings resulting from trimming (e.g., if input was just "'")
    validWords = filter (not . null) trimmedWords

    -- 6. Sort words alphabetically to prepare for grouping
    sortedWords = sort validWords

    -- 7. Group identical words together
    groupedWords = group sortedWords

    -- 8. Count occurrences of each word in its group
    wordCounts = map (\g -> (head g, length g)) groupedWords
