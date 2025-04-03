module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRIO)
import Data.Char (chr, ord, isLower)

-- Caesar Cipher Encode
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith shift text (cycle key)
  where
    shift char k = chr $ ((ord char - ord 'a' + shiftAmount k) `mod` 26) + ord 'a'
    shiftAmount k = ord k - ord 'a'

-- Caesar Cipher Decode
caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith unshift encodedText (cycle key)
  where
    unshift char k = chr $ ((ord char - ord 'a' - shiftAmount k + 26) `mod` 26) + ord 'a'
    shiftAmount k = ord k - ord 'a'

-- Generate a random key and encode the text
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  let encodedText = caesarEncode key text
  return (key, encodedText)

-- Helper function to generate a random key
generateRandomKey :: Int -> IO String
generateRandomKey n = mapM randomChar [1..n]
  where
    randomChar _ = randomRIO ('a', 'z')
