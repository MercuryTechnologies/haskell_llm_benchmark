module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower)
import Control.Monad (replicateM)

-- Helper: shift a single character by n positions (0-25)
shiftChar :: Int -> Char -> Char
shiftChar n c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'

-- Helper: unshift a single character by n positions (0-25)
unshiftChar :: Int -> Char -> Char
unshiftChar n c = chr $ ((ord c - ord 'a' - n + 26) `mod` 26) + ord 'a'

-- Caesar cipher encode with fixed shift of 3
caesarEncode :: String -> String -> String
caesarEncode _ text = map (shiftChar 3) text

-- Caesar cipher decode with fixed shift of 3
caesarDecode :: String -> String -> String
caesarDecode _ encodedText = map (unshiftChar 3) encodedText

-- General substitution cipher encode using key
encodeWithKey :: String -> String -> String
encodeWithKey key text = zipWith shiftByKey (cycle key) text
  where
    shiftByKey kChar tChar = shiftChar (ord kChar - ord 'a') tChar

-- General substitution cipher decode using key
decodeWithKey :: String -> String -> String
decodeWithKey key encodedText = zipWith unshiftByKey (cycle key) encodedText
  where
    unshiftByKey kChar eChar = unshiftChar (ord kChar - ord 'a') eChar

-- Generate a random lowercase letter
randomLowerChar :: IO Char
randomLowerChar = do
  n <- randomRIO (0,25)
  return $ chr (n + ord 'a')

-- Generate a random key of given length
generateRandomKey :: Int -> IO String
generateRandomKey len = replicateM len randomLowerChar

-- Caesar cipher encode with random key of length 100
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  let encoded = encodeWithKey key text
  return (key, encoded)
