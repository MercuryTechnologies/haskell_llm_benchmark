module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (chr, ord, isLower)
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- A helper function to convert a key character to its shift amount ('a' -> 0, 'b' -> 1, ..., 'z' -> 25)
charToShiftAmount :: Char -> Int
charToShiftAmount k = ord k - ord 'a'

-- Shift a character forward by the corresponding amount.
shiftForward :: Char -> Char -> Char
shiftForward c k
  | isLower c =
      let shiftAmount = charToShiftAmount k
          base        = ord 'a'
          oldPos      = ord c - base
          newPos      = (oldPos + shiftAmount) `mod` 26
      in chr (base + newPos)
  | otherwise = c

-- Shift a character backward by the corresponding amount.
shiftBackward :: Char -> Char -> Char
shiftBackward c k
  | isLower c =
      let shiftAmount = charToShiftAmount k
          base        = ord 'a'
          oldPos      = ord c - base
          newPos      = (oldPos - shiftAmount) `mod` 26
      in chr (base + newPos)
  | otherwise = c

-- Given a key (String) and some text, we encode by shifting forward according to each key character.
-- If the key is shorter than the text, it gets repeated (cycle).
caesarEncode :: String -> String -> String
caesarEncode key text =
  let fullKey = cycle key
  in zipWith shiftForward text fullKey

-- Decode does the inverse of Encode, i.e. shifting backward.
caesarDecode :: String -> String -> String
caesarDecode key encodedText =
  let fullKey = cycle key
  in zipWith shiftBackward encodedText fullKey

-- If a key is not provided, we generate a random one of length >= 100.
-- Then we encode the text using that random key and return both.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  let encoded = caesarEncode key text
  return (key, encoded)

-- Generate a random key of lowercase letters.
generateRandomKey :: Int -> IO String
generateRandomKey n = replicateM n (randomRIO ('a', 'z'))
