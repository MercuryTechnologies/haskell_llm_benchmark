module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = case cleanInput n of
    Just digits -> length digits > 1 && checksum digits `mod` 10 == 0
    Nothing -> False

cleanInput :: String -> Maybe [Int]
cleanInput s = 
    let filtered = filter (/= ' ') s
    in if all isDigit filtered
        then Just (map digitToInt filtered)
        else Nothing

checksum :: [Int] -> Int
checksum digits = sum (processDigits (reverse digits))

processDigits :: [Int] -> [Int]
processDigits [] = []
processDigits [x] = [x]
processDigits (x:y:xs) = x : doubleAndAdjust y : processDigits xs

doubleAndAdjust :: Int -> Int
doubleAndAdjust n
    | doubled > 9 = doubled - 9
    | otherwise = doubled
    where doubled = n * 2
