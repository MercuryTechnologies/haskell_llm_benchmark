module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isAsciiLower)
import System.Random (newStdGen, randomRs)

-- Calculate the shift amount (0-25) for a given key character.
-- Assumes the key character is a lowercase letter.
keyCharToShift :: Char -> Int
keyCharToShift c = ord c - ord 'a'

-- Apply a shift operation (addition for encode, subtraction for decode)
-- to a text character using a key character.
-- Only shifts lowercase letters; other characters are returned unchanged.
-- Assumes the key character is a valid lowercase letter.
shiftChar :: (Int -> Int -> Int) -> Char -> Char -> Char
shiftChar op keyChar textChar
  | isAsciiLower textChar =
      let shiftAmount = keyCharToShift keyChar
          textVal     = ord textChar - ord 'a'
          -- Apply operation and wrap around using modulo 26
          -- Ensure result is non-negative before final modulo for subtraction
          newVal      = (textVal `op` shiftAmount) `mod` 26
          finalVal    = if newVal < 0 then newVal + 26 else newVal
      in chr (ord 'a' + finalVal)
  | otherwise = textChar -- Pass non-lowercase characters through unchanged

-- Core logic for encoding/decoding using a key, cycling through the key characters.
-- Assumes the key contains only lowercase letters.
applyCipher :: (Int -> Int -> Int) -> String -> String -> String
applyCipher op key text = zipWith (shiftChar op) (cycle key) text

-- Encode plaintext using a substitution cipher with the given key.
-- caesarEncode "ddd" "iamapandabear" == "ldpdsdqgdehdu"
-- caesarEncode "abc" "iamapandabear" == "iboaranejcfbu"
caesarEncode :: String -> String -> String
caesarEncode = applyCipher (+)

-- Decode ciphertext using a substitution cipher with the given key.
-- caesarDecode "ddd" "ldpdsdqgdehdu" == "iamapandabear"
-- caesarDecode "abc" "iboaranejcfbu" == "iamapandabear"
caesarDecode :: String -> String -> String
caesarDecode = applyCipher (-)

-- Generate a random key of a specified length consisting of lowercase letters.
generateRandomKey :: Int -> IO String
generateRandomKey len = do
  gen <- newStdGen
  return $ take len $ randomRs ('a', 'z') gen

-- Encode plaintext with a randomly generated key of at least 100 characters.
-- Returns the generated key and the resulting ciphertext.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  let minKeyLength = 100
  key <- generateRandomKey minKeyLength
  -- caesarEncode assumes a valid key (lowercase), which generateRandomKey provides.
  -- caesarEncode handles non-lowercase chars in the text correctly.
  let encodedText = caesarEncode key text
  return (key, encodedText)
