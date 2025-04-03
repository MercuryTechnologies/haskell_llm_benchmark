module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random
import Data.Char (ord, chr, isLower)
import Data.List (zipWith)
import Control.Monad (replicateM)

-- Helper function to shift a character
shiftChar :: Int -> Char -> Char
shiftChar shift char
  | isLower char = chr $ (ord 'a' + (ord char - ord 'a' + shift) `mod` 26)
  | otherwise = char -- Ignore non-lowercase characters

-- Caesar Cipher Implementation
caesarEncode :: String -> String -> String
caesarEncode key text = encode key text
  where
    encode = caesarEncodeWithShift 3

caesarDecode :: String -> String -> String
caesarDecode key encodedText = decode key encodedText
  where
    decode = caesarDecodeWithShift 3

caesarEncodeWithShift :: Int -> String -> String -> String
caesarEncodeWithShift shift key text = map (shiftChar shift) text

caesarDecodeWithShift :: Int -> String -> String -> String
caesarDecodeWithShift shift key text = map (shiftChar (-shift)) text

-- Substitution Cipher Implementation
substitutionEncode :: String -> String -> String
substitutionEncode key text = zipWith shiftCharWithKey (cycle key) text

substitutionDecode :: String -> String -> String
substitutionDecode key text = zipWith shiftCharWithKey (cycle (map negateKeyChar key)) text

negateKeyChar :: Char -> Char
negateKeyChar c = chr $ ord 'a' + (ord 'z' - ord c)

shiftCharWithKey :: Char -> Char -> Char
shiftCharWithKey keyChar textChar
  | isLower textChar = shiftChar shift textChar
  | otherwise = textChar
  where
    shift = ord keyChar - ord 'a'

-- Random Key Generation
generateRandomKey :: Int -> IO String
generateRandomKey len = do
  randoms <- replicateM len (randomRIO ('a', 'z'))
  return randoms

-- Caesar Encode with Random Key
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey 100
  let encodedText = substitutionEncode key text
  return (key, encodedText)
