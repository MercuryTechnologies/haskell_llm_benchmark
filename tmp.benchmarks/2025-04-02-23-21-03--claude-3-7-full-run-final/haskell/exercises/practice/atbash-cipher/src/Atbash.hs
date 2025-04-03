module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- Substitutes a letter with its Atbash equivalent
substitute :: Char -> Char
substitute c
  | isAlpha c = toEnum (219 - fromEnum (toLower c)) -- 219 = 'a' + 'z' in ASCII
  | otherwise = c

-- Groups text into chunks of 5 characters
groupByFive :: String -> String
groupByFive [] = []
groupByFive xs
  | length chunk < 5 = chunk
  | otherwise        = chunk ++ " " ++ groupByFive rest
  where
    chunk = take 5 xs
    rest  = drop 5 xs

-- Encodes plain text to Atbash cipher
encode :: String -> String
encode plainText = groupByFive $ map substitute $ filter keepChar plainText
  where
    keepChar c = isAlpha c || isDigit c

-- Decodes Atbash cipher to plain text
decode :: String -> String
decode cipherText = filter (/= ' ') $ map substitute cipherText
