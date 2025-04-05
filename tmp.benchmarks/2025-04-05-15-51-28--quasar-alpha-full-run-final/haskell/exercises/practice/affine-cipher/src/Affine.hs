module Affine (decode, encode) where

import Data.Char (chr, isAlphaNum, isDigit, isLetter, ord, toLower)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)

m :: Int
m = 26

-- Check if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- Modular inverse of a mod m, if it exists
modInv :: Int -> Int -> Maybe Int
modInv a m
  | coprime a m = Just (modInv' a m)
  | otherwise = Nothing
  where
    modInv' a m = let (_, x, _) = extendedGCD a m in mod x m

-- Extended Euclidean Algorithm
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD 0 b = (b, 0, 1)
extendedGCD a b =
  let (g, s, t) = extendedGCD (b `mod` a) a
  in (g, t - (b `div` a) * s, s)

-- Convert letter to index 0-25
charToIndex :: Char -> Int
charToIndex c = ord (toLower c) - ord 'a'

-- Convert index 0-25 to letter
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Clean input: keep only letters and digits, lowercase
cleanInput :: String -> String
cleanInput = map toLower . filter isAlphaNum

-- Group string into chunks of n characters
groupN :: Int -> String -> [String]
groupN n = unfoldr (\s -> if null s then Nothing else Just (splitAt n s))

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a m) = Nothing
  | otherwise = Just $ unwords $ groupN 5 encoded
  where
    cleaned = cleanInput plainText
    encoded = concatMap encodeChar cleaned
    encodeChar c
      | isLetter c =
          let i = charToIndex c
              e = (a * i + b) `mod` m
          in [indexToChar e]
      | isDigit c = [c]
      | otherwise = ""

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  aInv <- modInv a m
  let cleaned = cleanInput cipherText
      decoded = map (decodeChar aInv b) cleaned
  return decoded

decodeChar :: Int -> Int -> Char -> Char
decodeChar aInv b c
  | isLetter c =
      let y = charToIndex c
          d = (aInv * (y - b)) `mod` m
          d' = if d < 0 then d + m else d
      in indexToChar d'
  | isDigit c = c
  | otherwise = c
