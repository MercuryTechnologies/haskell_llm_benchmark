module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

verse :: Int -> String
verse n
  | n > 2 = verseLine n (n - 1) "Take one down and pass it around"
  | n == 2 = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall."
  | n == 1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall."
  | n == 0 = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall."

verseLine :: Int -> Int -> String -> String
verseLine n m action =
  bottleCount n ++ " of beer on the wall, " ++ bottleCount n ++ " of beer.\n" ++
  action ++ ", " ++ bottleCount m ++ " of beer on the wall."

bottleCount :: Int -> String
bottleCount 0 = "no more bottles"
bottleCount 1 = "1 bottle"
bottleCount n = show n ++ " bottles"
