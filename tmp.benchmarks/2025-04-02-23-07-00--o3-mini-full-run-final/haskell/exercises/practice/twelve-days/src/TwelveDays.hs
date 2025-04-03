module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
  where
    days = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]
    gifts = [ "a Partridge in a Pear Tree."
            , "two Turtle Doves"
            , "three French Hens"
            , "four Calling Birds"
            , "five Gold Rings"
            , "six Geese-a-Laying"
            , "seven Swans-a-Swimming"
            , "eight Maids-a-Milking"
            , "nine Ladies Dancing"
            , "ten Lords-a-Leaping"
            , "eleven Pipers Piping"
            , "twelve Drummers Drumming"
            ]
    verse n =
      let dayStr = days !! (n - 1)
          currentGifts = reverse (take n gifts)
          giftList = if n > 1
                      then intercalate ", " (init currentGifts ++ [ "and " ++ last currentGifts ])
                      else head currentGifts
      in "On the " ++ dayStr ++ " day of Christmas my true love gave to me: " ++ giftList
