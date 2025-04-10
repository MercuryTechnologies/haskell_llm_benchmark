module Acronym (abbreviate) where

import Data.Char (isAlphaNum, toUpper, toLower)

abbreviate :: String -> String
abbreviate = map toUpper . map head . words' . filter (\c -> isAlphaNum c || c == '-')

words' :: String -> [String]
words' = map (map toLower) . words . map (\c -> if c == '-' then ' ' else c)
