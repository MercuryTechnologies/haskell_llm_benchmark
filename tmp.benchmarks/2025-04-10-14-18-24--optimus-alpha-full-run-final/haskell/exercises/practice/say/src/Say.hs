module Say (inEnglish) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ intercalate " " $ filter (not . null) $ sayChunks $ chunkThousands n

-- Step 2: Break number into chunks of thousands
chunkThousands :: Integer -> [Integer]
chunkThousands 0 = []
chunkThousands x = chunkThousands (x `div` 1000) ++ [x `mod` 1000]

-- Step 3: Scale words
scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"]

-- Step 4: Say each chunk with scale
sayChunks :: [Integer] -> [String]
sayChunks chunks =
  let
    n = length chunks
    pairs = zip chunks (reverse $ take n scales)
    sayPair (0, _) = ""
    sayPair (num, scale) =
      let chunkStr = sayBelowThousand num
      in if null scale then chunkStr else chunkStr ++ " " ++ scale
  in filter (not . null) $ map sayPair pairs

-- Step 1: Handle 0-99, and up to 999 for chunking
sayBelowThousand :: Integer -> String
sayBelowThousand n
  | n < 100 = sayBelowHundred n
  | otherwise =
      let (h, r) = n `divMod` 100
          rest = if r == 0 then "" else " " ++ sayBelowHundred r
      in ones !! fromIntegral h ++ " hundred" ++ rest

sayBelowHundred :: Integer -> String
sayBelowHundred n
  | n < 20 = ones !! fromIntegral n
  | otherwise =
      let (t, o) = n `divMod` 10
          hyphen = if o == 0 then "" else "-" ++ ones !! fromIntegral o
      in tens !! fromIntegral t ++ hyphen

ones :: [String]
ones =
  [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
  , "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
  , "sixteen", "seventeen", "eighteen", "nineteen"
  ]

tens :: [String]
tens =
  [ "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ]
