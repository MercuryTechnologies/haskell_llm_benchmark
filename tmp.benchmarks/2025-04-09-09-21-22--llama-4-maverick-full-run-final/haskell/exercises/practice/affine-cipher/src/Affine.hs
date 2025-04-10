module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord)

-- Function to check if two numbers are coprime
areCoprime :: Int -> Int -> Bool
areCoprime a m = gcd a m == 1

-- Modular multiplicative inverse
modularInverse :: Int -> Int -> Maybe Int
modularInverse a m
  | areCoprime a m = let (x, _) = egcd a m in Just (x `mod` m)
  | otherwise = Nothing

-- Extended Euclidean algorithm to find gcd and coefficients
egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = let (x, y) = egcd b (a `mod` b) in (y, x - (a `div` b) * y)

-- Affine cipher encryption
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (areCoprime a 26) = Nothing
  | otherwise = Just $ format $ filter isAlphaOrDigit $ map (encrypt a b) plainText

-- Affine cipher decryption
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (areCoprime a 26) = Nothing
  | otherwise = case mapM (decrypt a b) cipherText of
                  Just decrypted -> Just decrypted
                  Nothing -> Nothing

-- Encryption function
encrypt :: Int -> Int -> Char -> Char
encrypt a b c
  | isAlpha c = let i = ord (toLower c) - ord 'a' in toEnum (ord 'a' + (a * i + b) `mod` 26) :: Char
  | isDigit c = c
  | otherwise = ' '

-- Decryption function
decrypt :: Int -> Int -> Char -> Maybe Char
decrypt a b c
  | isAlpha c = let i = ord c - ord 'a'
                in case modularInverse a 26 of
                     Just aInv -> Just $ toEnum (ord 'a' + (aInv * (i - b)) `mod` 26) :: Char
                     Nothing -> Nothing
  | isDigit c = Just c
  | otherwise = Just ' '

-- Check if a character is alphanumeric
isAlphaOrDigit :: Char -> Bool
isAlphaOrDigit c = isAlpha c || isDigit c

-- Format the output into groups of 5 characters separated by spaces
format :: String -> String
format = unwords . chunk 5

-- Split a list into chunks of a specified size
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)
