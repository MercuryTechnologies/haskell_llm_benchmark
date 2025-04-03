module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (intercalate " " (filter (not . null) (map chunkToWords (zip [0..] (reverse (chunk n))))))

chunk :: Integer -> [Integer]
chunk 0 = []
chunk n = let (q, r) = n `divMod` 1000 in r : chunk q

chunkToWords :: (Int, Integer) -> String
chunkToWords (idx, n)
  | n == 0 = ""
  | otherwise = numberToWords n ++ if idx > 0 then " " ++ scale !! idx else ""
  where
    scale = ["", "thousand", "million", "billion", "trillion"]

numberToWords :: Integer -> String
numberToWords n
  | n < 20 = units !! fromIntegral n
  | n < 100 = let (q, r) = n `divMod` 10 in tens !! fromIntegral q ++ if r > 0 then "-" ++ units !! fromIntegral r else ""
  | otherwise = let (q, r) = n `divMod` 100 in units !! fromIntegral q ++ " hundred" ++ if r > 0 then " " ++ numberToWords r else ""
  where
    units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
             "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
