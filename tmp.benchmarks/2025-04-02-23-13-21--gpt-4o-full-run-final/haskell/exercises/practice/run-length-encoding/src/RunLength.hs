module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup :: String -> String
    encodeGroup xs = let len = length xs
                     in if len > 1 then show len ++ [head xs] else [head xs]

decode :: String -> String
decode encodedText = decodeHelper encodedText ""
  where
    decodeHelper :: String -> String -> String
    decodeHelper [] acc = acc
    decodeHelper (x:xs) acc
      | isDigit x = let (digits, rest) = span isDigit (x:xs)
                        count = read digits
                        (char:rest') = rest
                    in decodeHelper rest' (acc ++ replicate count char)
      | otherwise = decodeHelper xs (acc ++ [x])
