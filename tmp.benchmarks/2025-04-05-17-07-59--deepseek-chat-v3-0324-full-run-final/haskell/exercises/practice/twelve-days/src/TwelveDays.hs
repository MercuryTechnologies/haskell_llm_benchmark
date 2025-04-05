module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
  where
    verse day = "On the " ++ dayOrdinal day ++ " day of Christmas my true love gave to me: " ++ gifts day ++ "."
    
    dayOrdinal 1  = "first"
    dayOrdinal 2  = "second"
    dayOrdinal 3  = "third"
    dayOrdinal 4  = "fourth"
    dayOrdinal 5  = "fifth"
    dayOrdinal 6  = "sixth"
    dayOrdinal 7  = "seventh"
    dayOrdinal 8  = "eighth"
    dayOrdinal 9  = "ninth"
    dayOrdinal 10 = "tenth"
    dayOrdinal 11 = "eleventh"
    dayOrdinal 12 = "twelfth"
    
    gifts day = intercalate ", " (map gift [day,day-1..1])
    
    gift 1  = "a Partridge in a Pear Tree"
    gift 2  = "two Turtle Doves"
    gift 3  = "three French Hens"
    gift 4  = "four Calling Birds"
    gift 5  = "five Gold Rings"
    gift 6  = "six Geese-a-Laying"
    gift 7  = "seven Swans-a-Swimming"
    gift 8  = "eight Maids-a-Milking"
    gift 9  = "nine Ladies Dancing"
    gift 10 = "ten Lords-a-Leaping"
    gift 11 = "eleven Pipers Piping"
    gift 12 = "twelve Drummers Drumming"
    
    intercalate :: String -> [String] -> String
    intercalate _ []     = ""
    intercalate _ [x]    = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
