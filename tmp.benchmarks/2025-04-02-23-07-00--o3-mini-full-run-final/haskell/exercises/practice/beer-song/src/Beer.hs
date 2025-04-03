module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [99,98..1] ++ [finalVerse]) ++ "\n"
  where
    verse :: Int -> String
    verse n = firstLine n ++ "\n" ++ secondLine n
      where
        firstLine 1 = "1 bottle of beer on the wall, 1 bottle of beer."
        firstLine k = show k ++ " bottles of beer on the wall, " ++ show k ++ " bottles of beer."
        secondLine 1 = "Take it down and pass it around, no more bottles of beer on the wall."
        secondLine k =
          let remaining = k - 1
          in "Take one down and pass it around, " ++ drinkWord remaining ++ " of beer on the wall."
        drinkWord 1 = "1 bottle"
        drinkWord k = show k ++ " bottles"

    finalVerse :: String
    finalVerse = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall."
