module PigLatin (translate) where

import Data.Char (toLower, isAlpha)

-- Helper to check if a character is a vowel
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

-- Helper to check if a word starts with a given prefix
startsWith :: String -> String -> Bool
startsWith prefix str = take (length prefix) str == prefix

-- Find the index where the first vowel (or y as a vowel) occurs, with special rules for "qu"
firstVowelOrY :: String -> Int
firstVowelOrY w = go 0 w
  where
    go _ [] = 0
    go i (x:xs)
      | isVowel x = i
      | x == 'y' && i /= 0 = i
      | startsWith "qu" (x:xs) = i + 2
      | otherwise = go (i+1) xs

translateWord :: String -> String
translateWord word
  -- Rule 1: starts with vowel or "xr"/"yt"
  | isVowel (head word) = word ++ "ay"
  | startsWith "xr" word = word ++ "ay"
  | startsWith "yt" word = word ++ "ay"
  -- Rule 3: starts with consonant(s) + "qu"
  | let (pre, rest) = span (not . isVowel) word
  , startsWith "qu" rest = drop (length pre + 2) word ++ pre ++ "quay"
  -- Rule 2 & 4: move leading consonants (including y as vowel after first position)
  | otherwise =
      let idx = firstVowelOrY word
      in drop idx word ++ take idx word ++ "ay"

-- Split a string into words, translate each, and rejoin
translate :: String -> String
translate xs = unwords $ map translateWord (words xs)
