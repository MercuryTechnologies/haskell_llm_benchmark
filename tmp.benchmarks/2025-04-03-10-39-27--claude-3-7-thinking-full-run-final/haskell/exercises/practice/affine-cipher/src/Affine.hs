module Affine (decode, encode) where

import Data.Char (isLetter, isDigit, toLower, ord, chr)

-- Check if a and m are coprime (gcd(a,m) = 1)
areCoprime :: Int -> Int -> Bool
areCoprime a m = gcd a m == 1

-- Find the modular multiplicative inverse of a mod m
modularInverse :: Int -> Int -> Maybe Int
modularInverse a m
    | areCoprime a m = Just $ head [x | x <- [1..m-1], (a * x) `mod` m == 1]
    | otherwise = Nothing

-- Ensure a value is in the range [0, m-1]
modPositive :: Int -> Int -> Int
modPositive x m = ((x `mod` m) + m) `mod` m

-- Convert a letter to its numeric value (a=0, b=1, ..., z=25)
letterToNum :: Char -> Int
letterToNum c = ord (toLower c) - ord 'a'

-- Convert a numeric value to a letter (0=a, 1=b, ..., 25=z)
numToLetter :: Int -> Char
numToLetter n = chr ((n `mod` 26) + ord 'a')

-- Group a string into chunks of 5 characters
groupByFive :: String -> String
groupByFive [] = []
groupByFive s
    | length chunk < 5 = chunk
    | otherwise = chunk ++ " " ++ groupByFive rest
  where
    chunk = take 5 s
    rest = drop 5 s

-- Encode a string using the affine cipher
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
    | not (areCoprime a 26) = Nothing
    | otherwise = Just $ groupByFive $ map encryptChar $ filter (\c -> isLetter c || isDigit c) plainText
  where
    encryptChar c
        | isLetter c = numToLetter $ (a * letterToNum c + b) `mod` 26
        | otherwise = c  -- Preserve digits

-- Decode a string that was encoded with the affine cipher
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
    aInv <- modularInverse a 26
    return $ map (decryptChar aInv) $ filter (\c -> isLetter c || isDigit c) cipherText
  where
    decryptChar aInv c
        | isLetter c = numToLetter $ (aInv * modPositive (letterToNum c - b) 26) `mod` 26
        | otherwise = c  -- Preserve digits
