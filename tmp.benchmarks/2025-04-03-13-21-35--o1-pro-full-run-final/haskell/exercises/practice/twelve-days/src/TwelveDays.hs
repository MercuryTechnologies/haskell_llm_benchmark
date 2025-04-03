module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
  where
    verse n =
      "On the "
      ++ ordinal n
      ++ " day of Christmas my true love gave to me: "
      ++ gifts !! (n - 1)

ordinal :: Int -> String
ordinal n = ordinals !! (n - 1)
  where
    ordinals =
      [ "first"
      , "second"
      , "third"
      , "fourth"
      , "fifth"
      , "sixth"
      , "seventh"
      , "eighth"
      , "ninth"
      , "tenth"
      , "eleventh"
      , "twelfth"
      ]

gifts :: [String]
gifts =
  [ "a Partridge in a Pear Tree."
  , "two Turtle Doves, and a Partridge in a Pear Tree."
  , "three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  , "twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree."
  ]
