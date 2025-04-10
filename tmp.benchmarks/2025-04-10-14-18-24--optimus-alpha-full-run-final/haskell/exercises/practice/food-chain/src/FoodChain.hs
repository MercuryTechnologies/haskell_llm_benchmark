module FoodChain (song) where

import Data.List (intercalate)

data Animal = Animal
    { name :: String
    , comment :: Maybe String
    }

animals :: [Animal]
animals =
    [ Animal "fly"    Nothing
    , Animal "spider" (Just "It wriggled and jiggled and tickled inside her.")
    , Animal "bird"   (Just "How absurd to swallow a bird!")
    , Animal "cat"    (Just "Imagine that, to swallow a cat!")
    , Animal "dog"    (Just "What a hog, to swallow a dog!")
    , Animal "goat"   (Just "Just opened her throat and swallowed a goat!")
    , Animal "cow"    (Just "I don't know how she swallowed a cow!")
    , Animal "horse"  (Just "She's dead, of course!")
    ]

song :: String
song = intercalate "\n\n" $ map verse [0..length animals - 1]

verse :: Int -> String
verse n =
    let animal = animals !! n
        firstLine = "I know an old lady who swallowed a " ++ name animal ++ "."
        commentLine = maybe "" id (comment animal)
    in if name animal == "horse"
        then firstLine ++ "\n" ++ commentLine
        else intercalate "\n" $ filter (not . null)
            [ firstLine
            , commentLine
            , cumulativeLines n
            , ending
            ]

cumulativeLines :: Int -> String
cumulativeLines n
    | n == 0 = ""
    | otherwise = intercalate "\n" $ map swallowLine [n, n-1 .. 1]
  where
    swallowLine i =
        let curr = animals !! i
            prev = animals !! (i - 1)
            catchPhrase = case name prev of
                "spider" -> "spider that wriggled and jiggled and tickled inside her"
                _        -> name prev
        in "She swallowed the " ++ name curr ++ " to catch the " ++ catchPhrase ++ "."

ending :: String
ending = "I don't know why she swallowed the fly. Perhaps she'll die."
