module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (isLower)

-- For convenience, we'll assume the plaintext and key only contain lowercase letters [a-z].
-- If the key has any non-lowercase characters, we'll throw an error as described in the instructions.
-- Caesar cipher logic:
--  - 'a' corresponds to 0 shift, 'b' to 1, ..., 'z' to 25.
--  - Repeats the key if it's shorter than the text.

alphabetSize :: Int
alphabetSize = 26

charShift :: Char -> Int
charShift c = fromEnum c - fromEnum 'a'

encodeChar :: Char -> Int -> Char
encodeChar c shift =
  toEnum $ fromEnum 'a' + ((fromEnum c - fromEnum 'a' + shift) `mod` alphabetSize)

decodeChar :: Char -> Int -> Char
decodeChar c shift =
  toEnum $ fromEnum 'a' + ((fromEnum c - fromEnum 'a' - shift) `mod` alphabetSize)

expandKey :: String -> Int -> String
expandKey key n = take n (cycle key)

validateKey :: String -> String
validateKey k =
  if null k
    then error "Key is empty: use caesarEncodeRandom for random key."
  else if not (all isLower k)
    then error "Key must contain only lowercase letters."
  else k

-- | caesarEncode
-- Takes a key and a plaintext, returns encoded text.
caesarEncode :: String -> String -> String
caesarEncode key text =
  let validKey = validateKey key
      expanded = expandKey validKey (length text)
  in zipWith (\txtChr keyChr -> encodeChar txtChr (charShift keyChr)) text expanded

-- | caesarDecode
-- Takes a key and a ciphertext, returns the original text.
caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  let validKey = validateKey key
      expanded = expandKey validKey (length encodedText)
  in zipWith (\encChr keyChr -> decodeChar encChr (charShift keyChr)) encodedText expanded

-- | caesarEncodeRandom
-- Generates a random lowercase key of at least 100 chars
-- Returns (key, encodedText)
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  randKey <- mapM (\_ -> randomRIO ('a','z')) [1..100]
  let encodedText = caesarEncode randKey text
  return (randKey, encodedText)
