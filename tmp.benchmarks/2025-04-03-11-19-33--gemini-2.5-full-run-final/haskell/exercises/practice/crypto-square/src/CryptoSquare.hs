module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose, unwords)

-- Helper function to split a list into chunks of a given size.
-- This is often found in utility libraries but not in base.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Encodes a plaintext message using the Crypto Square cipher.
encode :: String -> String
encode xs
  -- If the normalized text is empty, return an empty string.
  | null normalized = ""
  -- Otherwise, proceed with encoding.
  | otherwise       = unwords $ transpose $ chunksOf c paddedText
  where
    -- 1. Normalize the input string:
    --    - Convert to lowercase.
    --    - Keep only alphanumeric characters.
    normalized :: String
    normalized = filter isAlphaNum $ map toLower xs

    -- Length of the normalized string.
    len :: Int
    len = length normalized

    -- 2. Calculate rectangle dimensions (columns c, rows r):
    --    - Find the smallest square size (c * c) >= len.
    --    - c = ceiling(sqrt(len))
    --    - r = ceiling(len / c)
    --    - This ensures c >= r and c - r <= 1.
    c :: Int
    c = ceiling $ sqrt (fromIntegral len :: Double)

    r :: Int
    -- Avoid division by zero if len is 0 (though handled by the guard).
    -- If c is 0, len must be 0, and r should also be 0.
    r = if c == 0 then 0 else ceiling (fromIntegral len / fromIntegral c :: Double)

    -- 3. Pad the normalized string with spaces:
    --    - The rectangle should have size r * c.
    --    - Add spaces to the end of normalized text to reach this length.
    paddedText :: String
    paddedText = normalized ++ replicate (r * c - len) ' '

    -- 4. Encode the message:
    --    a. `chunksOf c paddedText`: Breaks the padded text into `r` rows, each of length `c`.
    --       Example: ["ifmanwas", "meanttos", ..., "sroots  "]
    --    b. `transpose`: Transposes the rows into columns. The result is `c` columns, each of length `r`.
    --       Example: ["imtgdvs", "fearwer", ..., "sseoau "]
    --    c. `unwords`: Joins these columns (which are the ciphertext segments) with spaces.
    --       Example: "imtgdvs fearwer ... sseoau "
