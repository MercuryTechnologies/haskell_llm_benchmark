module Beer (song) where

song :: String
song = unlines $ map (++ "\n") $ map verse [99,98..0]

verse :: Int -> String
verse n = firstLine n ++ "\n" ++ secondLine n

firstLine :: Int -> String
firstLine n = bottles n True ++ " of beer on the wall, " ++ bottles n False ++ " of beer."

secondLine :: Int -> String
secondLine 0 = "Go to the store and buy some more, " ++ bottles 99 True ++ " of beer on the wall."
secondLine n = "Take " ++ it n ++ " down and pass it around, " ++ bottles (n-1) True ++ " of beer on the wall."

bottles :: Int -> Bool -> String
bottles 0 _ = "No more bottles"
bottles 1 _ = "1 bottle"
bottles n _ = show n ++ " bottles"

it :: Int -> String
it 1 = "it"
it _ = "one"
