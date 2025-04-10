module TwelveDays (recite) where

gifts :: [String]
gifts = [ "a Partridge in a Pear Tree"
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

verse :: Int -> String
verse n = "On the " ++ day n ++ " day of Christmas my true love gave to me: " ++ giftsList n ++ "."
  where
    day :: Int -> String
    day 1 = "first"
    day 2 = "second"
    day 3 = "third"
    day 4 = "fourth"
    day 5 = "fifth"
    day 6 = "sixth"
    day 7 = "seventh"
    day 8 = "eighth"
    day 9 = "ninth"
    day 10 = "tenth"
    day 11 = "eleventh"
    day 12 = "twelfth"
    day _ = error "Invalid day"

    giftsList :: Int -> String
    giftsList 1 = gifts !! 0
    giftsList n = concat [gifts !! (n-1), ", ", giftsList (n-1)]
                  |> (\s -> if n == 2 then init s ++ " and " ++ gifts !! 0 else s ++ ", and " ++ gifts !! 0)

    (|>) :: a -> (a -> b) -> b
    (|>) x f = f x

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
