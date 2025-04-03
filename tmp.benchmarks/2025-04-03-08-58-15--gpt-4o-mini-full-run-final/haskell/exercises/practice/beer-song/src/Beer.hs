module Beer (song) where

song :: String
song = unlines (map verse [99, 98 .. 1] ++ [finalVerse])

verse :: Int -> String
verse n = show n ++ " bottle" ++ (if n == 1 then "" else "s") ++ " of beer on the wall, " ++
           show n ++ " bottle" ++ (if n == 1 then "" else "s") ++ " of beer.\n" ++
           "Take one down and pass it around, " ++ (if n - 1 == 0 then "no more bottles" else show (n - 1) ++ " bottle" ++ (if n - 1 == 1 then "" else "s")) ++ " of beer on the wall.\n"

finalVerse :: String
finalVerse = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
              "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
