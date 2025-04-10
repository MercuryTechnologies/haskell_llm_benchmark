module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

-- The size of the alphabet
m :: Int
m = 26

-- Check if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Modular multiplicative inverse of a mod m
modInv :: Int -> Int -> Maybe Int
modInv a m = let a' = a `mod` m
                 inv = [x | x <- [1..m-1], (a' * x) `mod` m == 1]
             in case inv of
                  (x:_) -> Just x
                  []    -> Nothing

-- Convert a character to its alphabet index (0-based, a-z)
charToIndex :: Char -> Maybe Int
charToIndex c
  | isAlpha c = Just (ord (toLower c) - ord 'a')
  | otherwise = Nothing

-- Convert an index (0-based) to a character (a-z)
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Remove all non-alphanumeric characters, keep digits, convert to lowercase
normalize :: String -> String
normalize = map toLower . filter (\c -> isAlpha c || isDigit c)

-- Group a string into chunks of n characters
groupN :: Int -> String -> [String]
groupN n = unfoldr (\s -> if null s then Nothing else Just (splitAt n s))

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a m) = Nothing
  | otherwise =
      let norm = normalize plainText
          enc = map (encodeChar (a, b)) norm
      in Just $ unwords $ groupN 5 enc

encodeChar :: (Int, Int) -> Char -> Char
encodeChar (a, b) c
  | isAlpha c =
      let i = fromMaybe 0 (charToIndex c)
          e = (a * i + b) `mod` m
      in indexToChar e
  | isDigit c = c
  | otherwise = error "Unexpected character in encodeChar"

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (coprime a m) = Nothing
  | otherwise =
      case modInv a m of
        Nothing -> Nothing
        Just aInv ->
          let norm = map toLower $ filter (\c -> isAlpha c || isDigit c) cipherText
              dec = map (decodeChar (aInv, b)) norm
          in Just dec

decodeChar :: (Int, Int) -> Char -> Char
decodeChar (aInv, b) c
  | isAlpha c =
      let y = fromMaybe 0 (charToIndex c)
          d = (aInv * (y - b)) `mod` m
          d' = if d < 0 then d + m else d
      in indexToChar d'
  | isDigit c = c
  | otherwise = error "Unexpected character in decodeChar"
