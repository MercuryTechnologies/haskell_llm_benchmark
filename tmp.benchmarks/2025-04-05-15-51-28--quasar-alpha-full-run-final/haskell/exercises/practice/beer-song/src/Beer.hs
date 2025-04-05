module Beer (song) where

song :: String
song = concatMap verse [99,98..0]

verse :: Int -> String
verse n = start ++ " of beer on the wall, " ++ bottles n ++ " of beer.\n" ++
          action n ++ ", " ++ bottles next ++ " of beer on the wall.\n" ++
          (if n /= 0 then "\n" else "")
  where
    next = if n == 0 then 99 else n - 1
    start = if n == 0 then capitalize (bottles n) else bottles n

bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"

action :: Int -> String
action 0 = "Go to the store and buy some more"
action 1 = "Take it down and pass it around"
action _ = "Take one down and pass it around"

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs
  where
    toUpper c
      | 'a' <= c && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c
