song :: String
song = unlines [verse n | n <- [99, 98..1]] ++ "\nNo more bottles of beer on the wall, no more bottles of beer.\n\
              \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
