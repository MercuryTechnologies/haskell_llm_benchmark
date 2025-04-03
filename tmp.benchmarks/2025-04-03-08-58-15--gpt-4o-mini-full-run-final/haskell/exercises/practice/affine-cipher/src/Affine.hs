module Affine (decode, encode) where

import Data.Char (isAlpha, ord, chr, toLower)
import Data.Maybe (fromMaybe)

-- Function to check if two numbers are coprime
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Function to find the modular multiplicative inverse
modInverse :: Int -> Int -> Maybe Int
modInverse a m = findMMI 1
  where
    findMMI x
      | (a * x) `mod` m == 1 = Just x
      | x >= m = Nothing
      | otherwise = findMMI (x + 1)

-- Encode function
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | gcd' a 26 /= 1 = Nothing
  | otherwise = Just $ concatMap encodeChar (filter isAlpha plainText)
  where
    encodeChar c = let
      i = ord (toLower c) - ord 'a'
      e = (a * i + b) `mod` 26
      in [chr (e + ord 'a')]

-- Decode function
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | gcd' a 26 /= 1 = Nothing
  | otherwise = Just $ concatMap decodeChar (filter isAlpha cipherText)
  where
    mmi = fromMaybe 0 (modInverse a 26)
    decodeChar c = let
      y = ord (toLower c) - ord 'a'
      d = (mmi * (y - b)) `mod` 26
      in [chr (d + ord 'a')]
