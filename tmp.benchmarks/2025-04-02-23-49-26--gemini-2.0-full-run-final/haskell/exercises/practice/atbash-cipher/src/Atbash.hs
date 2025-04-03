module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isLower, ord, chr)

atbash :: Char -> Char
atbash c
  | isLower c = chr $ ord 'a' + ord 'z' - ord c
  | otherwise = c

encode :: String -> String
encode plainText =
  let encoded = map (\c -> if isAlphaNum c then (atbash $ toLower c) else c) plainText
      filtered = filter isAlphaNum encoded
      grouped = unwords $ groupBy5 filtered
  in  grouped
  where
    toLower c = if 'A' <= c && c <= 'Z' then chr (ord c + 32) else c
    groupBy5 str =
      case length str of
        0 -> []
        n -> let (prefix, suffix) = splitAt (min 5 n) str
             in  prefix : groupBy5 suffix

decode :: String -> String
decode cipherText = map (\c -> if isLower c then atbash c else c) $ filter isAlphaNum $ map toLower cipherText
  where
    toLower c = if 'A' <= c && c <= 'Z' then chr (ord c + 32) else c
