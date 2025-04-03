module Affine (decode, encode) where

import Data.Char (isAlpha, isUpper, ord, chr, toLower)
import Data.Maybe (mapMaybe)

-- | Encode plaintext using the affine cipher with key (a,b)
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | gcd a 26 /= 1 = Nothing  -- a and 26 must be coprime
  | otherwise = Just $ formatOutput $ mapMaybe (encodeChar (a, b)) (filter isAlpha plainText)

-- | Decode ciphertext using the affine cipher with key (a,b)
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | gcd a 26 /= 1 = Nothing  -- a and 26 must be coprime
  | otherwise = Just $ map (decodeChar (a, b)) (filter isAlpha cipherText)

-- | Format output in groups of 5 letters separated by spaces
formatOutput :: String -> String
formatOutput [] = []
formatOutput xs
  | length xs <= 5 = xs
  | otherwise = take 5 xs ++ " " ++ formatOutput (drop 5 xs)

-- | Encode a single character using the affine cipher
encodeChar :: (Int, Int) -> Char -> Maybe Char
encodeChar (a, b) c
  | isAlpha c = Just $ encodeAlpha (a, b) c
  | otherwise = Nothing

-- | Encode an alphabetic character
encodeAlpha :: (Int, Int) -> Char -> Char
encodeAlpha (a, b) c = 
  let base = ord 'a'  -- Always use lowercase base
      i = ord (toLower c) - base
      encoded = (a * i + b) `mod` 26
  in chr (encoded + base)  -- Always return lowercase

-- | Decode a single character using the affine cipher
decodeChar :: (Int, Int) -> Char -> Char
decodeChar (a, b) c
  | isAlpha c = decodeAlpha (a, b) c
  | otherwise = c

-- | Decode an alphabetic character
decodeAlpha :: (Int, Int) -> Char -> Char
decodeAlpha (a, b) c =
  let base = if isUpper c then ord 'A' else ord 'a'
      y = ord c - base
      aInverse = modInverse a 26
      decoded = (aInverse * (y - b `mod` 26)) `mod` 26
  in chr (decoded + base)

-- | Calculate the modular multiplicative inverse
modInverse :: Int -> Int -> Int
modInverse a m = 
  let (_, x, _) = extendedGcd a m
  in (x `mod` m + m) `mod` m

-- | Extended Euclidean Algorithm to find gcd(a,b) and coefficients x,y such that ax + by = gcd(a,b)
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b =
  let (g, x, y) = extendedGcd b (a `mod` b)
  in (g, y, x - (a `div` b) * y)
