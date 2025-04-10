module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower)
import Control.Monad (replicateM)

-- Helper: shift a single lowercase letter by n positions, wrapping around 'a'..'z'
shiftChar :: Int -> Char -> Char
shiftChar n c
  | isLower c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
  | otherwise = c

-- Helper: unshift a single lowercase letter by n positions, wrapping around 'a'..'z'
unshiftChar :: Int -> Char -> Char
unshiftChar n c = shiftChar (-n) c

-- Caesar cipher: shift by 3
caesarEncode :: String -> String -> String
caesarEncode _ text = map (shiftChar 3) text

caesarDecode :: String -> String -> String
caesarDecode _ text = map (unshiftChar 3) text

-- Substitution cipher: key is a string of lowercase letters, each letter is a shift
-- If key is shorter than text, repeat key as needed
encodeWithKey :: String -> String -> String
encodeWithKey key text = zipWith shiftChar shifts text
  where
    key' = cycle key
    shifts = map (\k -> ord k - ord 'a') key'

decodeWithKey :: String -> String -> String
decodeWithKey key text = zipWith unshiftChar shifts text
  where
    key' = cycle key
    shifts = map (\k -> ord k - ord 'a') key'

-- caesarEncodeRandom: if key is empty, generate a random key of at least 100 lowercase letters
-- Returns (key, encodedText)
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- randomKey (max 100 (length text))
  let encoded = encodeWithKey key text
  return (key, encoded)

-- Helper: generate a random key of given length, only lowercase letters
randomKey :: Int -> IO String
randomKey n = replicateM n randomLower
  where
    randomLower = do
      i <- randomRIO (0,25)
      return $ chr (ord 'a' + i)
