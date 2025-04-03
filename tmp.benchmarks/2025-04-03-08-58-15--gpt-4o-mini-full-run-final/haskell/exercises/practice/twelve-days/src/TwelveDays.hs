module TwelveDays (recite) where

import qualified Data.Text as T
import           Data.Text (Text)

recite :: Int -> Int -> [Text]
recite start stop = map (T.pack . verse) [start..stop]
  where
    verse n = concat $ 
      ["On the ", ordinal n, " day of Christmas my true love gave to me: "] ++
      (reverse $ gifts n) ++ ["."]
    
    gifts 1 = ["a Partridge in a Pear Tree"]
    gifts 2 = ["two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 3 = ["three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 4 = ["four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 5 = ["five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 6 = ["six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 7 = ["seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 8 = ["eight Maids-a-Milking", "seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 9 = ["nine Ladies Dancing", "eight Maids-a-Milking", "seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 10 = ["ten Lords-a-Leaping", "nine Ladies Dancing", "eight Maids-a-Milking", "seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 11 = ["eleven Pipers Piping", "ten Lords-a-Leaping", "nine Ladies Dancing", "eight Maids-a-Milking", "seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]
    gifts 12 = ["twelve Drummers Drumming", "eleven Pipers Piping", "ten Lords-a-Leaping", "nine Ladies Dancing", "eight Maids-a-Milking", "seven Swans-a-Swimming", "six Geese-a-Laying", "five Gold Rings", "four Calling Birds", "three French Hens", "two Turtle Doves", "and a Partridge in a Pear Tree"]

    ordinal n = case n of
      1 -> "first"
      2 -> "second"
      3 -> "third"
      4 -> "fourth"
      5 -> "fifth"
      6 -> "sixth"
      7 -> "seventh"
      8 -> "eighth"
      9 -> "ninth"
      10 -> "tenth"
      11 -> "eleventh"
      12 -> "twelfth"
      _ -> ""
