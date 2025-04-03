module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, toLower, ord, chr)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (first, rest) = splitAt n xs in first : chunksOf n rest

atbash :: Char -> Char
atbash c
  | isAlpha c = chr (ord 'a' + (ord 'z' - ord (toLower c)))
  | otherwise = c

decode :: String -> String
decode cipherText = map atbash (filter isAlphaNum cipherText)

encode :: String -> String
encode plainText = unwords . chunksOf 5 $ map atbash (filter isAlphaNum plainText)
