module TwelveDays (recite) where

import Data.List (intercalate, init, last) -- Added init and last

days :: [String]
days = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

gifts :: [String]
gifts = [
    "a Partridge in a Pear Tree",
    "two Turtle Doves",
    "three French Hens",
    "four Calling Birds",
    "five Gold Rings",
    "six Geese-a-Laying",
    "seven Swans-a-Swimming",
    "eight Maids-a-Milking",
    "nine Ladies Dancing",
    "ten Lords-a-Leaping",
    "eleven Pipers Piping",
    "twelve Drummers Drumming"
    ]

-- Generates the verse for a single day n (1-based index)
verse :: Int -> String
verse n
  | n <= 0 || n > 12 = error "Day must be between 1 and 12"
  | otherwise = intro ++ giftList n ++ "."
  where
    dayOrdinal = days !! (n - 1)
    intro = "On the " ++ dayOrdinal ++ " day of Christmas my true love gave to me: "

-- Generates the formatted list of gifts for day n
giftList :: Int -> String
giftList n =
    let relevantGifts = reverse $ take n gifts -- Gifts from day n down to day 1. e.g., n=3 -> ["three French Hens", "two Turtle Doves", "a Partridge in a Pear Tree"]
    in case relevantGifts of
        []      -> "" -- Should not happen with valid n >= 1
        [gift1] -> gift1 -- Day 1: "a Partridge in a Pear Tree"
        _       -> intercalate ", " (init relevantGifts) ++ ", and " ++ last relevantGifts -- Days 2+: "gift n, ..., gift 2, and gift 1"

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
