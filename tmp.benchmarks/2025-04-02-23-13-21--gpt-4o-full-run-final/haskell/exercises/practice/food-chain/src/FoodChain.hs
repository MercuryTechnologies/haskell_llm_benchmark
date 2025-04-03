module FoodChain (song) where

song :: String
song = unlines $ concatMap verse [0..length animals - 1]

verse :: Int -> [String]
verse n = intro n : middle n ++ [outro n]

intro :: Int -> String
intro n = "I know an old lady who swallowed a " ++ animal n ++ "."

middle :: Int -> [String]
middle 0 = [reason 0]
middle n = (reason n : map action (reverse [1..n])) ++ [reason 0]

outro :: Int -> String
outro n
    | n == length animals - 1 = "She's dead, of course!"
    | otherwise = ""

animal :: Int -> String
animal n = animals !! n

reason :: Int -> String
reason n = reasons !! n

action :: Int -> String
action n = "She swallowed the " ++ animal n ++ " to catch the " ++ animal (n - 1) ++ (if n == 2 then " that wriggled and jiggled and tickled inside her." else ".")

animals :: [String]
animals = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]

reasons :: [String]
reasons = [ "I don't know why she swallowed the fly. Perhaps she'll die."
          , "It wriggled and jiggled and tickled inside her."
          , "How absurd to swallow a bird!"
          , "Imagine that, to swallow a cat!"
          , "What a hog, to swallow a dog!"
          , "Just opened her throat and swallowed a goat!"
          , "I don't know how she swallowed a cow!"
          , ""  -- Adding an empty string for the horse, as it doesn't have a reason
          ]
