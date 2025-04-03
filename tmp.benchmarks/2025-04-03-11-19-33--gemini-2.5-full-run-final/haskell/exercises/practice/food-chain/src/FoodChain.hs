module FoodChain (song) where

import Data.List (intercalate)

-- Represents (animal_name, unique_second_line_or_ending)
animals :: [(String, String)]
animals =
  [ ("fly",    "I don't know why she swallowed the fly. Perhaps she'll die.") -- Special first verse ending
  , ("spider", "It wriggled and jiggled and tickled inside her.")
  , ("bird",   "How absurd to swallow a bird!")
  , ("cat",    "Imagine that, to swallow a cat!")
  , ("dog",    "What a hog, to swallow a dog!")
  , ("goat",   "Just opened her throat and swallowed a goat!")
  , ("cow",    "I don't know how she swallowed a cow!")
  , ("horse",  "She's dead, of course!") -- Special last verse ending
  ]

numAnimals :: Int
numAnimals = length animals

-- Generates the lines "She swallowed the X to catch the Y..."
-- for the verse introducing the animal at index `verseIndex`.
chain :: Int -> [String]
chain verseIndex = go verseIndex
  where
    go :: Int -> [String]
    go 0 = [] -- Base case: stop before the fly
    go currentAnimalIndex =
      let (swallower, _) = animals !! currentAnimalIndex
          (prey, _)      = animals !! (currentAnimalIndex - 1)
          line = "She swallowed the " ++ swallower ++ " to catch the " ++ prey ++ ending
          ending = case prey of
                     "spider" -> " that wriggled and jiggled and tickled inside her."
                     _        -> "."
      in line : go (currentAnimalIndex - 1)

-- Generates a single verse for the animal at the given index (0-based).
verse :: Int -> String
verse index =
  let (animalName, uniqueLineOrEnding) = animals !! index
      line1 = "I know an old lady who swallowed a " ++ animalName ++ "."
  in case animalName of
       "fly"   -> intercalate "\n" [line1, uniqueLineOrEnding]
       "horse" -> intercalate "\n" [line1, uniqueLineOrEnding]
       _       -> intercalate "\n" $ [line1, uniqueLineOrEnding] ++ chain index ++ [snd (animals !! 0)] -- Add standard fly ending

-- Generates the entire song by combining all verses.
song :: String
song = intercalate "\n\n" (map verse [0 .. numAnimals - 1]) ++ "\n"
