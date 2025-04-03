module Beer (song) where

song :: String
song = unlines $ map verse [99, 98 .. 1] ++ [finalVerse]

verse :: Int -> String
verse n = 
    let currentBottles = bottleCount n
        nextBottles = bottleCount (n - 1)
        action = if n == 1 then "Take it down and pass it around" else "Take one down and pass it around"
    in currentBottles ++ " of beer on the wall, " ++ currentBottles ++ " of beer.\n" ++
       action ++ ", " ++ nextBottles ++ " of beer on the wall.\n"

bottleCount :: Int -> String
bottleCount 0 = "no more bottles"
bottleCount 1 = "1 bottle"
bottleCount n = show n ++ " bottles"

finalVerse :: String
finalVerse = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
             "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
