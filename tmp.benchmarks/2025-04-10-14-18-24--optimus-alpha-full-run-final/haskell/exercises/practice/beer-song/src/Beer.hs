module Beer (song) where

import Data.Char (toUpper)

song :: String
song = concatMap verse [99,98..0]

verse :: Int -> String
verse n = firstLine n ++ "\n" ++ secondLine n ++ "\n" ++ endLine n

firstLine :: Int -> String
firstLine n = capitalize (bottles n ++ " of beer on the wall, " ++ bottles n ++ " of beer.")

secondLine :: Int -> String
secondLine n
  | n > 1     = "Take one down and pass it around, " ++ bottles (n-1) ++ " of beer on the wall."
  | n == 1    = "Take it down and pass it around, no more bottles of beer on the wall."
  | n == 0    = "Go to the store and buy some more, 99 bottles of beer on the wall."
  | otherwise = error "Negative bottles not allowed"

endLine :: Int -> String
endLine 0 = ""
endLine _ = "\n"

bottles :: Int -> String
bottles n
  | n > 1     = show n ++ " bottles"
  | n == 1    = "1 bottle"
  | n == 0    = "no more bottles"
  | otherwise = error "Negative bottles not allowed"

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs
