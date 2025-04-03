module FoodChain (song) where

-- | List of animals in order of appearance in the song
animals :: [String]
animals = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]

-- | Special comment for each animal (in the same order as animals)
comments :: [String]
comments = [
    "I don't know why she swallowed the fly. Perhaps she'll die.",
    "It wriggled and jiggled and tickled inside her.",
    "How absurd to swallow a bird!",
    "Imagine that, to swallow a cat!",
    "What a hog, to swallow a dog!",
    "Just opened her throat and swallowed a goat!",
    "I don't know how she swallowed a cow!",
    "She's dead, of course!"
  ]

-- | Generate a verse for the given animal index
verse :: Int -> String
verse 0 = "I know an old lady who swallowed a fly.\n" ++
         "I don't know why she swallowed the fly. Perhaps she'll die."
verse 7 = "I know an old lady who swallowed a horse.\n" ++
         "She's dead, of course!"
verse n = "I know an old lady who swallowed a " ++ animals !! n ++ ".\n" ++
         comments !! n ++ "\n" ++
         concatMap chainLine [n, n-1 .. 1] ++
         "I don't know why she swallowed the fly. Perhaps she'll die."

-- | Generate a single line in the swallowing chain
chainLine :: Int -> String
chainLine 1 = "She swallowed the spider to catch the fly.\n"
chainLine 2 = "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n"
chainLine n = "She swallowed the " ++ animals !! n ++ " to catch the " ++ animals !! (n-1) ++ ".\n"

-- | Generate the complete song by combining all verses
song :: String
song = unlines $ map verse [0..7]
