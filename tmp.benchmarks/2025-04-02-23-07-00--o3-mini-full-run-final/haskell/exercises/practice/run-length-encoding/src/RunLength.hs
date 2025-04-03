module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit)

decode :: String -> String
decode "" = ""
decode s =
  let (numStr, rest) = span isDigit s
  in case rest of
       (c:cs) ->
         let count = if null numStr then 1 else read numStr
         in replicate count c ++ decode cs
       [] -> ""

encode :: String -> String
encode s = concatMap encodeGroup (group s)
  where
    encodeGroup xs@(c:_)
      | length xs == 1 = [c]
      | otherwise      = show (length xs) ++ [c]
    encodeGroup [] = ""
