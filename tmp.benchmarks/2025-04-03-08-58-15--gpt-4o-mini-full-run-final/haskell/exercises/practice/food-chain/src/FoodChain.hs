module FoodChain (song) where

animals :: [(String, String)]
animals =
    [ ("fly", "I don't know why she swallowed the fly. Perhaps she'll die.")
    , ("spider", "It wriggled and jiggled and tickled inside her.")
    , ("bird", "How absurd to swallow a bird!")
    , ("cat", "Imagine that, to swallow a cat!")
    , ("dog", "What a hog, to swallow a dog!")
    , ("goat", "Just opened her throat and swallowed a goat!")
    , ("cow", "I don't know how she swallowed a cow!")
    , ("horse", "She's dead, of course!")
    ]

verse :: String -> String
verse animal =
    "I know an old lady who swallowed a " ++ animal ++ ".\n" ++
    (if animal == "horse" then snd (animals !! 7) else snd (animals !! index animal)) ++ "\n" ++
    concatMap (\a -> "She swallowed the " ++ a ++ " to catch the " ++ previousAnimal a ++ ".\n") (reverse (takeWhile (/= animal) (map fst animals))) ++
    (if animal /= "horse" then snd (animals !! index animal) ++ "\n" else "")

previousAnimal :: String -> String
previousAnimal animal = case lookup animal animals of
    Just _ -> animal
    Nothing -> ""

index :: String -> Int
index animal = case lookup animal (zip (map fst animals) [0..]) of
    Just i -> i
    Nothing -> error "Animal not found"

song :: String
song = concatMap verse (map fst animals)
