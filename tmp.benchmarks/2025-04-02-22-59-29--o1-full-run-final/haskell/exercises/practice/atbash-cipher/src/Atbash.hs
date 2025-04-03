module Atbash (decode, encode) where

import Data.Char (chr, ord, toLower, isAlpha, isDigit)

-- | Decode a string using the Atbash cipher.
--   Punctuation and spaces are ignored.
--   Letters are inverted a->z, b->y, etc.
--   Digits remain unchanged.
decode :: String -> String
decode txt =
  let filtered = filterAndDowncase txt
  in map atbash filtered

-- | Encode a string using the Atbash cipher,
--   then output the result in groups of 5 characters.
--   Punctuation and spaces are ignored.
--   Letters are inverted a->z, b->y, etc.
--   Digits remain unchanged.
encode :: String -> String
encode txt =
  let filtered = filterAndDowncase txt
      transformed = map atbash filtered
  in chunkBy5 transformed

-- | Convert letters to lowercase and remove anything
--   that's not a letter or digit.
filterAndDowncase :: String -> String
filterAndDowncase = map toLower . filter (\c -> isAlpha c || isDigit c)

-- | Perform the Atbash substitution on a single character.
--   Letters are inverted a->z, b->y, etc.
--   Digits remain as is.
atbash :: Char -> Char
atbash c
  | isAlpha c = chr (ord 'z' - (ord c - ord 'a'))
  | isDigit c = c
  | otherwise = c

-- | Split the string into chunks of 5 characters,
--   separated by spaces.
chunkBy5 :: String -> String
chunkBy5 [] = []
chunkBy5 s  =
  unwords (go s)
  where
    go [] = []
    go xs = let (groupOf5, rest) = splitAt 5 xs
            in groupOf5 : go rest
