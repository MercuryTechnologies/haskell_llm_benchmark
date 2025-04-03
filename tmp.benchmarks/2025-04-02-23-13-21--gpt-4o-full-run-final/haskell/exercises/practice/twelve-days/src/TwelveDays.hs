module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
  where
    verse n = "On the " ++ days !! (n - 1) ++ " day of Christmas my true love gave to me: " ++ gifts n
    days = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]
    gifts n = concatMap (\i -> if i == 1 && n > 1 then "and " ++ gift i else gift i) [n, n-1..1]
    gift 1 = "a Partridge in a Pear Tree."
    gift 2 = "two Turtle Doves, "
    gift 3 = "three French Hens, "
    gift 4 = "four Calling Birds, "
    gift 5 = "five Gold Rings, "
    gift 6 = "six Geese-a-Laying, "
    gift 7 = "seven Swans-a-Swimming, "
    gift 8 = "eight Maids-a-Milking, "
    gift 9 = "nine Ladies Dancing, "
    gift 10 = "ten Lords-a-Leaping, "
    gift 11 = "eleven Pipers Piping, "
    gift 12 = "twelve Drummers Drumming, "
