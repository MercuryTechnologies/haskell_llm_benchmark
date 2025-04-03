module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- Helper to split a list into chunks of size n
-- Not strictly necessary to define here as Data.List.Split has it,
-- but avoiding adding dependencies as requested.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Applies Atbash substitution to a letter (case-insensitive),
-- returns digits unchanged, and returns Nothing for other characters.
transformChar :: Char -> Maybe Char
transformChar c
    | isAlpha c = Just $ chr (ord 'a' + ord 'z' - ord (toLower c))
    | isDigit c = Just c
    | otherwise = Nothing

-- Decodes Atbash ciphertext.
-- Filters out non-alphanumeric characters (including spaces used for grouping)
-- and applies the Atbash transformation to letters.
decode :: String -> String
decode cipherText = mapMaybe transformChar cipherText

-- Encodes plaintext using Atbash cipher.
-- Filters out non-alphanumeric characters, applies the transformation,
-- and groups the result into blocks of 5 characters separated by spaces.
encode :: String -> String
encode plainText = intercalate " " $ chunksOf 5 transformedChars
  where
    -- Process the input: filter non-alphanumeric and transform letters/digits
    transformedChars = mapMaybe transformChar plainText
