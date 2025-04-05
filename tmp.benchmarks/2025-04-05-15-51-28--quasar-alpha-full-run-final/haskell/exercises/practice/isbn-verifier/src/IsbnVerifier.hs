module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn s =
  let clean = filter (/= '-') s
  in case clean of
       [a,b,c,d,e,f,g,h,i,j] ->
         -- 'X' is only valid as the last character
         if (any isX [a,b,c,d,e,f,g,h,i]) then False
         else
           let digits = mapM charToDigit [a,b,c,d,e,f,g,h,i,j]
           in case digits of
                Just ds ->
                  let weights = [10,9..1]
                      total = sum $ zipWith (*) ds weights
                  in total `mod` 11 == 0
                Nothing -> False
       _ -> False

isX :: Char -> Bool
isX c = c == 'X' || c == 'x'

charToDigit :: Char -> Maybe Int
charToDigit c
  | isDigit c = Just (digitToInt c)
  | isX c = Just 10
  | otherwise = Nothing
