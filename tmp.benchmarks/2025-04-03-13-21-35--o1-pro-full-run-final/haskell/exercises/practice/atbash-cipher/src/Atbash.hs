module Atbash (decode, encode) where

import Data.Char (chr, ord, isAlpha, isDigit, toLower)

decode :: String -> String
decode cipherText =
  let filtered = filter (\c -> isAlpha c || isDigit c) (map toLower cipherText)
  in map atbashChar filtered

encode :: String -> String
encode plainText =
  let filtered = filter (\c -> isAlpha c || isDigit c) (map toLower plainText)
      transformed = map atbashChar filtered
      groups = groupN 5 transformed
  in unwords groups

atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = chr (ord 'z' + ord 'a' - ord c)
  | otherwise = c

groupN :: Int -> String -> [String]
groupN _ "" = []
groupN n s = take n s : groupN n (drop n s)
