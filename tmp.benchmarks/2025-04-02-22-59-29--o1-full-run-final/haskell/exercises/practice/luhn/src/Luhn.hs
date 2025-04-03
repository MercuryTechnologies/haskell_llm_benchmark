module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid input =
    let noSpaces = filter (/= ' ') input
    in  if length noSpaces <= 1
        then False
        else if any (not . isDigit) noSpaces
             then False
             else (luhnSum noSpaces `mod` 10) == 0

luhnSum :: String -> Int
luhnSum s =
    sum $
    zipWith (\c i ->
        let d = digitToInt c
        in if odd i
           then let dd = d * 2
                in if dd > 9 then dd - 9 else dd
           else d
    ) (reverse s) [0..]
