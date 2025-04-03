module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- Helper function to map a character to its Atbash equivalent
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = toEnum (219 - fromEnum (toLower c))  -- 219 = 'a' + 'z'
  | otherwise = c

-- Helper function to remove non-alphanumeric characters
filterText :: String -> String
filterText = filter (\c -> isAlpha c || isDigit c)

-- Helper function to group text into blocks of 5 characters
groupText :: String -> String
groupText s = unwords (group 5 s)
  where
    group _ [] = []
    group n xs = take n xs : group n (drop n xs)

-- Encode function
encode :: String -> String
encode plainText = groupText $ map atbashChar $ filterText plainText

-- Decode function
decode :: String -> String
decode cipherText = map atbashChar $ filterText cipherText
