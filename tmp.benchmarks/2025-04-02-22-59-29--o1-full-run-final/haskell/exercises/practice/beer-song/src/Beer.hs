module Beer (song) where

song :: String
song = concatMap verse [99,98..0]
  where
    verse n
      | n > 2 = createVerse (show n ++ " bottles of beer") (show (n - 1) ++ " bottles of beer")
      | n == 2 = createVerse "2 bottles of beer" "1 bottle of beer"
      | n == 1 =
          "1 bottle of beer on the wall, 1 bottle of beer.\n" ++
          "Take it down and pass it around, no more bottles of beer on the wall.\n\n"
      | n == 0 =
          "No more bottles of beer on the wall, no more bottles of beer.\n" ++
          "Go to the store and buy some more, 99 bottles of beer on the wall.\n"

    createVerse xBeer nextBeer =
      xBeer ++ " on the wall, " ++ xBeer ++ ".\n" ++
      "Take one down and pass it around, " ++ nextBeer ++ " on the wall.\n\n"
