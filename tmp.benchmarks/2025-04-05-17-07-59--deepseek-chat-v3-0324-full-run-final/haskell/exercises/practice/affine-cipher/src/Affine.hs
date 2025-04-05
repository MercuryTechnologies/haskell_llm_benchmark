module Affine (decode, encode) where

import Data.Char (isLetter, isDigit, toLower, ord, chr)
import Data.List (find)

-- Constants
alphabetSize :: Int
alphabetSize = 26

-- Check if two numbers are coprime
areCoprime :: Int -> Int -> Bool
areCoprime a b = gcd a b == 1

-- Find modular multiplicative inverse
mmi :: Int -> Int -> Maybe Int
mmi a m = find (\x -> (a * x) `mod` m == 1) [1..m-1]

-- Convert char to position (0-25)
charToPos :: Char -> Maybe Int
charToPos c
  | isLetter c = Just $ ord (toLower c) - ord 'a'
  | otherwise = Nothing

-- Convert position to char
posToChar :: Int -> Char
posToChar n = chr (n + ord 'a')

-- Process text with affine transformation
processText :: (Int -> Int -> Int -> Int) -> (Int, Int) -> String -> Maybe String
processText transform (a, b) text = do
  -- Check if a and m are coprime
  if not (areCoprime a alphabetSize)
    then Nothing
    else Just $ concatMap (processChar a b) text
  where
    processChar :: Int -> Int -> Char -> Char
    processChar a b c
      | isLetter c = case charToPos c of
          Just i -> posToChar $ transform a b i
          Nothing -> c
      | isDigit c = c
      | otherwise = ' '  -- Replace non-alphanumeric with space

-- Encode a single character position
encodeChar :: Int -> Int -> Int -> Int
encodeChar a b i = (a * i + b) `mod` alphabetSize

-- Decode a single character position
decodeChar :: Int -> Int -> Int -> Int
decodeChar a b i = case mmi a alphabetSize of
  Just inv -> (inv * (i - b)) `mod` alphabetSize
  Nothing -> error "Shouldn't happen since we check coprimality first"

-- Group into 5-character chunks
groupText :: String -> String
groupText = unwords . map (take 5) . takeWhile (not . null) . iterate (drop 5)

decode :: (Int, Int) -> String -> Maybe String
decode key cipherText = processText decodeChar key (filter (/= ' ') cipherText)

encode :: (Int, Int) -> String -> Maybe String
encode key plainText = do
  processed <- processText encodeChar key plainText
  return $ groupText processed
