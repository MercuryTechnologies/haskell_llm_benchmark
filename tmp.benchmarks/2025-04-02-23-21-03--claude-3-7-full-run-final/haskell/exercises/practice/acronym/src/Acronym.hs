module Acronym (abbreviate) where

import Data.Char (toUpper, isAlpha, isSpace, isUpper)

abbreviate :: String -> String
abbreviate = map toUpper . concatMap firstLetter . splitCamelCase . words . replaceHyphens . filter validChar
  where
    -- Replace hyphens with spaces to treat them as word separators
    replaceHyphens = map (\c -> if c == '-' then ' ' else c)
    
    -- Keep only alphanumeric characters, spaces, and hyphens
    validChar c = isAlpha c || isSpace c || c == '-'
    
    -- Get the first letter of each word
    firstLetter [] = []
    firstLetter (x:_) | isAlpha x = [x]
                      | otherwise = []
    
    -- Split camelCase words into separate words
    splitCamelCase [] = []
    splitCamelCase [x] = [[x]]
    splitCamelCase (x:y:xs)
      | not (isUpper x) && isUpper y = [x] : splitCamelCase (y:xs)
      | otherwise = let (w:ws) = splitCamelCase (y:xs) in (x:w):ws
