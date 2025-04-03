module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)
import Data.Function (on)

-- Function to split the text into words
splitWords :: String -> [String]
splitWords [] = []
splitWords xs = let (word, rest) = break (not . isAlphaNum) xs
                    (_, remaining) = span (not . isAlphaNum) rest
                in if null word then splitWords remaining else word : splitWords remaining

-- Function to normalize words by converting to lowercase
normalizeWord :: String -> String
normalizeWord = map toLower

-- Main function to count words
wordCount :: String -> [(String, Int)]
wordCount xs = map (\ws -> (head ws, length ws)) . group . sort . map normalizeWord . splitWords $ xs
