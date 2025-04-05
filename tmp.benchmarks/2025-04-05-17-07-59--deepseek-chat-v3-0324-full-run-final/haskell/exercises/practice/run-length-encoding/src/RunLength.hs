module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode encodedText = 
    let (countStr, rest1) = span isDigit encodedText
        count = if null countStr then 1 else read countStr
        (char, rest2) = case rest1 of
                         [] -> (' ', [])  -- shouldn't happen per problem statement
                         (c:cs) -> (c, cs)
    in replicate count char ++ decode rest2

encode :: String -> String
encode [] = []
encode (x:xs) = 
    let (run, rest) = span (== x) xs
        count = length run + 1
    in if count > 1 
       then show count ++ [x] ++ encode rest
       else [x] ++ encode rest
