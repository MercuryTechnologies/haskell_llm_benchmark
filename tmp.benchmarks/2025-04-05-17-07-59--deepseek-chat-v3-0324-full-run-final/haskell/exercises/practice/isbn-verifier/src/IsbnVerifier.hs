module IsbnVerifier (isbn) where

import Data.Char (isDigit, toUpper)

isbn :: String -> Bool
isbn s = 
    let cleaned = filter (/= '-') s
        validFormat = length cleaned == 10 && 
                      all isDigit (init cleaned) && 
                      (isDigit (last cleaned) || toUpper (last cleaned) == 'X')
    in if not validFormat 
        then False
        else 
            let digits = map toDigit (init cleaned) ++ [checkDigit (last cleaned)]
                weights = [10,9..1]
                weightedSum = sum $ zipWith (*) digits weights
            in weightedSum `mod` 11 == 0
    where
        toDigit c = read [c] :: Int
        checkDigit c 
            | isDigit c = toDigit c
            | otherwise = 10
