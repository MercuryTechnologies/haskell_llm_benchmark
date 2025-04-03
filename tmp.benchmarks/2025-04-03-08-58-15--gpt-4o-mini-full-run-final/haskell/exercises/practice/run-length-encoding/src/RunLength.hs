module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode (x:xs) = replicate count x ++ decode rest
  where (countStr, rest) = span isDigit xs
        count = read countStr :: Int

encode :: String -> String
encode [] = []
encode (x:xs) = let (same, rest) = span (== x) (x:xs)
                in show (length same) ++ [x] ++ encode rest
