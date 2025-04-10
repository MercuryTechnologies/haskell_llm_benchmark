module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLetter, toLower, ord, chr)

-- Helper: Atbash substitution for a single character
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = chr (ord 'a' + (25 - (ord (toLower c) - ord 'a')))
  | otherwise = c

-- Helper: Remove all non-alphanumeric characters, convert to lowercase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Helper: Group a string into chunks of n
groupN :: Int -> String -> [String]
groupN _ [] = []
groupN n xs = let (g, rest) = splitAt n xs in g : groupN n rest

encode :: String -> String
encode plainText =
  let normalized = normalize plainText
      encoded = map (\c -> if isAlpha c then atbashChar c else c) normalized
      grouped = unwords (groupN 5 encoded)
  in grouped

decode :: String -> String
decode cipherText =
  let normalized = filter isAlphaNum cipherText
      decoded = map (\c -> if isAlpha c then atbashChar c else c) normalized
  in decoded
