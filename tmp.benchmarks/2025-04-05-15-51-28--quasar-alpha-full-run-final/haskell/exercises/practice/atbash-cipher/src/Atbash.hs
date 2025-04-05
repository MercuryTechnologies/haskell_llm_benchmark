module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLetter, toLower)

-- Map a single character using Atbash cipher
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = toEnum (fromEnum 'a' + (25 - (fromEnum (toLower c) - fromEnum 'a')))
  | otherwise = c

-- Remove all non-alphanumeric characters, convert to lowercase, and apply Atbash cipher
encode :: String -> String
encode plainText = unwords (group5 (go plainText))
  where
    go [] = []
    go (c:cs)
      | isAlphaNum c =
          let ch = if isAlpha c then atbashChar c else c
          in toLower ch : go cs
      | otherwise = go cs

-- Group a string into chunks of 5 characters
group5 :: String -> [String]
group5 [] = []
group5 s = take 5 s : group5 (drop 5 s)

-- Decode: ignore spaces, apply Atbash cipher, keep digits, remove punctuation
decode :: String -> String
decode cipherText = map decodeChar (filter isAlphaNum cipherText)
  where
    decodeChar c
      | isAlpha c = atbashChar c
      | otherwise = c
