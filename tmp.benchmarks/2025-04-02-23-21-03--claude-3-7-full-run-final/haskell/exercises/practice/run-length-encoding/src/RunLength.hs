module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode encodedText = decodeChunk encodedText
  where
    decodeChunk [] = []
    decodeChunk str = 
      let (countStr, rest) = span isDigit str
          count = if null countStr then 1 else read countStr
          (char:remainingStr) = rest
      in replicate count char ++ decodeChunk remainingStr

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup xs@(x:_) = 
      let count = length xs
      in (if count == 1 then "" else show count) ++ [x]
    encodeGroup [] = []
