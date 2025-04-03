module Affine (decode, encode) where

import Data.Char (isAlphaNum, isDigit, isLower, toLower, ord, chr)
import Data.List (intercalate)
-- gcd is in Prelude

-- Alphabet size
m :: Int
m = 26

-- Check coprimality using Prelude's gcd
areCoprime :: Int -> Int -> Bool
areCoprime a n = gcd a n == 1

-- Modular Multiplicative Inverse (simple search for small m)
-- Returns Nothing if a and n are not coprime.
mmi :: Int -> Int -> Maybe Int
mmi a n
  | not (areCoprime a n) = Nothing
  | otherwise = Just $ head [x | x <- [1..n-1], (a * x) `mod` n == 1]

-- Ensure result of mod is non-negative
-- Standard Haskell `mod` behaves correctly for negative dividends,
-- but this makes the logic explicit if needed elsewhere or for clarity.
-- However, `mod` is sufficient here. Let's use a simpler helper or rely on `mod`.
-- Let's stick with standard `mod` and ensure inputs to it are handled correctly.
-- The formula (mmi * (y - b)) `mod` m works correctly in Haskell.

-- Helper to split into chunks (implementing manually as it's not in base Data.List)
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Encode function
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (areCoprime a m) = Nothing
  | otherwise = Just $ formatCiphertext $ map processChar $ filter isAlphaNum $ map toLower plainText
  where
    processChar :: Char -> Char
    processChar c
      | isDigit c = c
      | isLower c = -- We ensured lowercase above
          let i = ord c - ord 'a'
              encodedIndex = (a * i + b) `mod` m -- Haskell's mod handles this correctly
          in chr (encodedIndex + ord 'a')
      | otherwise = c -- Should not happen due to filtering

    -- Group into chunks of 5
    formatCiphertext :: String -> String
    formatCiphertext = intercalate " " . chunksOf 5


-- Decode function
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText =
  case mmi a m of
    Nothing -> Nothing -- MMI doesn't exist if not coprime
    Just mmiVal -> Just $ map processChar $ filter isAlphaNum cipherText -- Filter out spaces and punctuation
      where
        processChar :: Char -> Char
        processChar c
          | isDigit c = c
          | isLower c = -- Assume input letters are lowercase after filtering
              let y = ord c - ord 'a'
                  -- Haskell's `mod` handles negative results correctly for modular arithmetic
                  -- e.g., (-1) `mod` 26 == 25
                  decodedIndex = (mmiVal * (y - b)) `mod` m
              in chr (decodedIndex + ord 'a')
          -- Handle potential uppercase letters in input, though examples suggest lowercase
          | otherwise = c -- Should only be digits or lowercase letters after filtering
