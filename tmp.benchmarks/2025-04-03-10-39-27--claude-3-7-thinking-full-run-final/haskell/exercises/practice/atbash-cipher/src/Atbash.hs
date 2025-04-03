module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, isSpace)

-- Convert a character to its Atbash equivalent
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = toEnum (fromEnum 'z' - fromEnum (toLower c) + fromEnum 'a')
  | otherwise = c

-- Keep only valid characters (letters and numbers)
isValidChar :: Char -> Bool
isValidChar c = isAlpha c || isDigit c || isSpace c

-- Clean input by removing punctuation and converting to lowercase
cleanInput :: String -> String
cleanInput = filter isValidChar . map toLower

-- Group text into chunks of 5 characters
groupBy5 :: String -> String
groupBy5 [] = []
groupBy5 xs
  | length chunk < 5 = chunk
  | null rest        = chunk  -- No trailing space if this is the last chunk
  | otherwise        = chunk ++ " " ++ groupBy5 rest
  where
    chunk = take 5 xs
    rest = drop 5 xs

encode :: String -> String
encode plainText = groupBy5 $ map atbashChar $ filter (\c -> isAlpha c || isDigit c) $ cleanInput plainText

decode :: String -> String
decode cipherText = map atbashChar $ filter (\c -> isAlpha c || isDigit c) $ cleanInput cipherText
