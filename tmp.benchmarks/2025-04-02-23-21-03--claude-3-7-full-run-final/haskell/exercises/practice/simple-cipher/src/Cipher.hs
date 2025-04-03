module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower)
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Helper function to shift a character by a given amount
shift :: Char -> Int -> Char
shift c n
  | isLower c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
  | otherwise = c

-- Helper function to get shift amount from a key character
keyShift :: Char -> Int
keyShift c = ord c - ord 'a'

-- Encode text using a key (substitution cipher)
caesarEncode :: String -> String -> String
caesarEncode "" _ = ""
caesarEncode _ "" = ""
caesarEncode key text = zipWith shiftChar text (cycle key)
  where
    shiftChar c k = shift c (keyShift k)

-- Decode text using a key (substitution cipher)
caesarDecode :: String -> String -> String
caesarDecode "" _ = ""
caesarDecode _ "" = ""
caesarDecode key encodedText = zipWith shiftChar encodedText (cycle key)
  where
    shiftChar c k = shift c (26 - keyShift k)

-- Generate a random key of at least 100 lowercase letters and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey (max 100 (length text))
  return (key, caesarEncode key text)

-- Helper function to generate a random key of specified length
generateRandomKey :: Int -> IO String
generateRandomKey len = replicateM len (randomRIO ('a', 'z'))
