module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower, toLower)

-- Helper function to shift a character by n positions
shiftChar :: Int -> Char -> Char
shiftChar n c
    | isLower c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
    | otherwise = c

-- Encode text using a key (each char in key specifies shift amount)
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith shiftChar shifts text
  where
    shifts = map (\c -> ord c - ord 'a') (cycle $ map toLower key)

-- Decode text using a key (reverse of encoding)
caesarDecode :: String -> String -> String
caesarDecode key text = zipWith shiftChar (map negate shifts) text
  where
    shifts = map (\c -> ord c - ord 'a') (cycle $ map toLower key)

-- Generate a random key and encode the text with it
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- generateRandomKey (max 100 (length text))
    return (key, caesarEncode key text)
  where
    generateRandomKey :: Int -> IO String
    generateRandomKey len = sequence $ replicate len (randomRIO ('a', 'z'))
