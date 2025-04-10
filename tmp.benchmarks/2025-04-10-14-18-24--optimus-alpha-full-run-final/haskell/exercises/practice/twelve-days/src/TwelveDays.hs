module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]

verse :: Int -> String
verse n =
    "On the " ++ ordinals !! (n - 1) ++ " day of Christmas my true love gave to me: " ++
    giftsLine n ++ "."

ordinals :: [String]
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
    [ "a Partridge in a Pear Tree"
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

giftsLine :: Int -> String
giftsLine 1 = gifts !! 0
giftsLine n =
    let gs = reverse (take n gifts)
        gs' = case gs of
                (x:xs) -> x : map (\g -> g) xs
                [] -> []
        -- The last gift should be prefixed with "and "
        gs'' = case gs' of
                (x:xs) -> reverse (("and " ++ last gs') : init gs')
                [] -> []
    in if n == 1
        then gifts !! 0
        else concatGifts (init gs) ++ ", and " ++ last gs

concatGifts :: [String] -> String
concatGifts [] = ""
concatGifts [x] = x
concatGifts (x:xs) = x ++ ", " ++ concatGifts xs
