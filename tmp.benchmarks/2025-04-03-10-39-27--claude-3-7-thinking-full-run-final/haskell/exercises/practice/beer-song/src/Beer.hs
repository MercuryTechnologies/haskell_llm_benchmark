module Beer (song) where

import Data.List (intercalate)

-- Helper function to handle the plural form
bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"

-- Generate a verse
verse :: Int -> String
verse n =
  let firstBottles = if n == 0 then "No more bottles" else bottles n
      secondBottles = bottles n
      action = if n > 0
               then "Take " ++ (if n == 1 then "it" else "one") ++ " down and pass it around"
               else "Go to the store and buy some more"
      nextBottles = bottles (if n > 0 then n - 1 else 99)
      
      firstLine = firstBottles ++ " of beer on the wall, " ++ secondBottles ++ " of beer.\n"
      secondLine = action ++ ", " ++ nextBottles ++ " of beer on the wall."
  in firstLine ++ secondLine

-- Generate the complete song
song :: String
song = intercalate "\n\n" [verse n | n <- [99,98..0]] ++ "\n"
