module RunLength (decode, encode) where

import Data.Char (isDigit, digitToInt)
import Data.List (group)
import Data.List (span)

encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup xs
      | length xs == 1 = [head xs]
      | otherwise = show (length xs) ++ [head xs]

decode :: String -> String
decode "" = ""
decode s =
  let (digits, rest) = span isDigit s
      count = if null digits then 1 else read digits
      c = head rest
      remaining = tail rest
  in replicate count c ++ decode remaining
