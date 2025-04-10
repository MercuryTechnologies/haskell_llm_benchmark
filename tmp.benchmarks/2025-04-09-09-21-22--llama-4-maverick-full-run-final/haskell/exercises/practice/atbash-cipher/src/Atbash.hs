module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- Function to apply Atbash substitution to a character
atbash :: Char -> Char
atbash c
  | isAlpha c = toLower $ chr $ ord 'a' + ord 'z' - ord c
  | isDigit c = c
  | otherwise = ' ' -- Ignore non-alphanumeric characters

-- Encode a string using Atbash cipher
encode :: String -> String
encode = unwords . chunk 5 . filter (/= ' ') . map atbash . map toLower

-- Decode a string using Atbash cipher
decode :: String -> String
decode = filter (/= ' ') . map atbash

-- Helper function to chunk a list into sublists of a given size
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Helper functions to work with ASCII values
ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum
