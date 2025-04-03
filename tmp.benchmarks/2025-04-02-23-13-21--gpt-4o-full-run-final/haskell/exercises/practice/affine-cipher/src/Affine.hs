module Affine (decode, encode) where

import Data.Char (chr, isAlpha, isDigit, ord, toLower)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T

alphabet :: String
alphabet = ['a'..'z']

m :: Int
m = length alphabet

-- Calculate the modular multiplicative inverse of a mod m
modInverse :: Int -> Int -> Maybe Int
modInverse a m = findInverse 1
  where
    findInverse x
      | (a * x) `mod` m == 1 = Just x
      | x >= m = Nothing
      | otherwise = findInverse (x + 1)

-- Check if two numbers are coprime
areCoprime :: Int -> Int -> Bool
areCoprime a b = gcd a b == 1

-- Encode a single character
encodeChar :: (Int, Int) -> Char -> Char
encodeChar (a, b) char
  | isAlpha char = chr $ ((a * index + b) `mod` m) + ord 'a'
  | otherwise = char
  where
    index = fromMaybe 0 (elemIndex (toLower char) alphabet)

-- Decode a single character
decodeChar :: (Int, Int) -> Char -> Char
decodeChar (a, b) char
  | isAlpha char = chr $ ((aInv * (index - b)) `mod` m) + ord 'a'
  | otherwise = char
  where
    index = fromMaybe 0 (elemIndex (toLower char) alphabet)
    aInv = fromMaybe 1 (modInverse a m)

-- Encode a string using the affine cipher
encode :: (Int, Int) -> Text -> Maybe Text
encode (a, b) plainText
  | not (areCoprime a m) = Nothing
  | otherwise = Just $ T.pack $ map (encodeChar (a, b)) (T.unpack plainText)

-- Decode a string using the affine cipher
decode :: (Int, Int) -> Text -> Maybe Text
decode (a, b) cipherText
  | not (areCoprime a m) = Nothing
  | otherwise = Just $ T.pack $ map (decodeChar (a, b)) (T.unpack cipherText)
