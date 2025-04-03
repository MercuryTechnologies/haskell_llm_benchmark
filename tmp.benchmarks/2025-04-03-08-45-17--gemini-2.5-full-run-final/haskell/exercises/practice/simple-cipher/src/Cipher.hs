module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower, toLower)
import System.Random (randomRIO, getStdGen, randomRs)

-- Convert 'a'..'z' to 0..25
charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

-- Convert 0..25 to 'a'..'z'
intToChar :: Int -> Char
intToChar n = chr (ord 'a' + n)

-- Normalize string: lowercase and keep only letters a-z
normalize :: String -> String
normalize = map toLower . filter isLower

-- Apply a shift (positive for encode, negative for decode)
applyShift :: Int -> Char -> Char -> Char
applyShift direction keyChar textChar =
  let keyOffset = charToInt keyChar
      textInt   = charToInt textChar
      -- Add 26 before modulo for decoding to handle negative results correctly
      shiftedInt = (textInt + direction * keyOffset + 26) `mod` 26
  in intToChar shiftedInt

-- Core cipher logic using a shifter function
processCipher :: (Char -> Char -> Char) -> String -> String -> String
processCipher shifter key text =
  let normalizedText = normalize text
      normalizedKey  = normalize key
      -- Ensure key is not empty, default to 'a' perhaps?
      -- For now, assume valid key as per function type.
      -- Cycle through the key characters to match the text length
      cycledKey = cycle normalizedKey
  in zipWith shifter cycledKey normalizedText

-- Encode text using the key
caesarEncode :: String -> String -> String
caesarEncode key text = processCipher (applyShift 1) key text

-- Decode text using the key
caesarDecode :: String -> String -> String
caesarDecode key encodedText = processCipher (applyShift (-1)) key encodedText

-- Generate a random key and encode text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  gen <- getStdGen
  -- Generate a random key of exactly 100 lowercase characters
  let keyLength = 100
  let randomKey = take keyLength $ randomRs ('a', 'z') gen
  -- Encode the text using the generated key
  let encodedText = caesarEncode randomKey text
  return (randomKey, encodedText)
