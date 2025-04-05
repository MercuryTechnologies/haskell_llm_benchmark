module FoodChain (song) where

song :: String
song = unlines $ concatMap verse [0..7]

verse :: Int -> [String]
verse n = case n of
    0 -> [intro "fly", ending, ""]
    1 -> [intro "spider",
          "It wriggled and jiggled and tickled inside her.",
          swallow "spider" "fly" True,
          ending,
          ""]
    2 -> [intro "bird",
          "How absurd to swallow a bird!",
          swallow "bird" "spider" True,
          swallow "spider" "fly" False,
          ending,
          ""]
    3 -> [intro "cat",
          "Imagine that, to swallow a cat!",
          swallow "cat" "bird" False,
          swallow "bird" "spider" True,
          swallow "spider" "fly" False,
          ending,
          ""]
    4 -> [intro "dog",
          "What a hog, to swallow a dog!",
          swallow "dog" "cat" False,
          swallow "cat" "bird" False,
          swallow "bird" "spider" True,
          swallow "spider" "fly" False,
          ending,
          ""]
    5 -> [intro "goat",
          "Just opened her throat and swallowed a goat!",
          swallow "goat" "dog" False,
          swallow "dog" "cat" False,
          swallow "cat" "bird" False,
          swallow "bird" "spider" True,
          swallow "spider" "fly" False,
          ending,
          ""]
    6 -> [intro "cow",
          "I don't know how she swallowed a cow!",
          swallow "cow" "goat" False,
          swallow "goat" "dog" False,
          swallow "dog" "cat" False,
          swallow "cat" "bird" False,
          swallow "bird" "spider" True,
          swallow "spider" "fly" False,
          ending,
          ""]
    7 -> [intro "horse",
          "She's dead, of course!"]
    _ -> []

intro :: String -> String
intro animal = "I know an old lady who swallowed a " ++ animal ++ "."

swallow :: String -> String -> Bool -> String
swallow predator prey addSpiderPhrase =
    "She swallowed the " ++ predator ++ " to catch the " ++ prey ++ suffix
  where
    suffix = if addSpiderPhrase && prey == "spider"
             then " that wriggled and jiggled and tickled inside her."
             else "."

ending :: String
ending = "I don't know why she swallowed the fly. Perhaps she'll die."
