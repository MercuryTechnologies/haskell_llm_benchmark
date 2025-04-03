module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (toLower, ord, chr)
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Validate and normalize the key ensuring it contains only lowercase letters.
validateKey :: String -> String
validateKey [] = error "Key cannot be empty. Use caesarEncodeRandom for automatic key generation."
validateKey k =
  let k' = map toLower k
  in if all (\c -> c >= 'a' && c <= 'z') k'
      then k'
      else error "Key must contain only letters 'a' to 'z'."

-- Shift a given character by the provided integer (which may be negative for decoding).
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | c >= 'a' && c <= 'z' =
      let base = ord 'a'
          offset = ord c - base
          newOffset = (offset + shift) `mod` 26
      in chr (base + newOffset)
  | otherwise = c

-- Apply the cipher by processing each character in the text with the corresponding key character (cycling through the key).
applyCipher :: (Int -> Int) -> String -> String -> String
applyCipher shiftFunction key text = zipWith cipher text (cycle keyShifts)
  where
    keyShifts = map (\c -> shiftFunction (ord c - ord 'a')) (validateKey key)
    cipher ch shiftAmt = shiftChar shiftAmt ch

-- For encoding, we add the key's shift values.
caesarEncode :: String -> String -> String
caesarEncode key text = applyCipher id key text

-- For decoding, we subtract the key's shift values.
caesarDecode :: String -> String -> String
caesarDecode key encodedText = applyCipher negate key encodedText

-- Generate a random key of the specified length consisting of lowercase letters.
randomKey :: Int -> IO String
randomKey n = replicateM n (randomRIO ('a', 'z'))

-- If no key is provided, this function will generate a random key of 100 characters then encode the text.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey 100
  return (key, caesarEncode key text)
