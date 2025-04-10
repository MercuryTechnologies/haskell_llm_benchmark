module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)
import Data.List (span)

encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup xs =
      let len = length xs
      in (if len == 1 then "" else show len) ++ [head xs]

decode :: String -> String
decode "" = ""
decode s  =
  let (digits, rest) = span isDigit s
  in case rest of
       [] -> ""
       (c:cs) ->
         let count = if null digits then 1 else read digits
         in replicate count c ++ decode cs
