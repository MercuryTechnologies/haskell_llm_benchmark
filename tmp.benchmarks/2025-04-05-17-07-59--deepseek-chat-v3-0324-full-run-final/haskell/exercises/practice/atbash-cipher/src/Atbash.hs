module Atbash (decode, encode) where

import Data.Char (toLower, isLetter, isDigit, isSpace)

-- Create the Atbash mapping
atbashMap :: [(Char, Char)]
atbashMap = zip ['a'..'z'] ['z','y'..'a']

-- Helper to transform a single character
transformChar :: Char -> Char
transformChar c
    | isLetter c = case lookup (toLower c) atbashMap of
                    Just x  -> x
                    Nothing -> c
    | isDigit c = c
    | otherwise = ' '  -- Convert other characters to space for filtering

-- Process text: lowercase, keep letters/numbers, remove others
cleanText :: String -> String
cleanText = filter (\c -> isLetter c || isDigit c) . map toLower

-- Group into 5-character chunks
group5 :: String -> String
group5 [] = []
group5 s
    | length s <= 5 = s
    | otherwise = take 5 s ++ " " ++ group5 (drop 5 s)

decode :: String -> String
decode cipherText = map transformChar $ filter (not . isSpace) cipherText

encode :: String -> String
encode plainText = group5 $ map transformChar $ cleanText plainText
