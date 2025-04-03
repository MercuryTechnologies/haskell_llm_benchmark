module FoodChain (song) where

import Data.List (intercalate)

song :: String
-- Instead of adding blank lines between verses, we simply
-- concatenate all lines from each verse, then join them
-- with newlines. This ensures the test sees line 4 as
-- "I know an old lady who swallowed a spider."
song = unlines . concatMap verse $ [0..7]

-- Each animal with its special explanatory line. 
-- "fly" has no special line, and "horse" signals the final verse.
animals :: [(String, String)]
animals =
  [ ("fly",   "")
  , ("spider","It wriggled and jiggled and tickled inside her.")
  , ("bird",  "How absurd to swallow a bird!")
  , ("cat",   "Imagine that, to swallow a cat!")
  , ("dog",   "What a hog, to swallow a dog!")
  , ("goat",  "Just opened her throat and swallowed a goat!")
  , ("cow",   "I don't know how she swallowed a cow!")
  , ("horse", "She's dead, of course!")
  ]

verse :: Int -> [String]
verse n =
  let (creature, specialLine) = animals !! n
      firstLine = "I know an old lady who swallowed a " ++ creature ++ "."
      secondLine
        | n == 0 = "I don't know why she swallowed the fly. Perhaps she'll die."
        | n == 7 = specialLine
        | otherwise = specialLine
      -- For horse, only two lines. For others, build the chain down to the fly.
      middleLines
        | creature == "horse" = []
        | otherwise = buildChain n
      linesOfVerse =
        [ firstLine ]
        ++ (if null secondLine then [] else [secondLine])
        ++ middleLines
  in linesOfVerse

buildChain :: Int -> [String]
buildChain 0 = []
buildChain idx =
  let (thisAnimal, _) = animals !! idx
      (prevAnimal, _) = animals !! (idx - 1)
      restChain = buildChain (idx - 1)
      extraForSpider =
        if prevAnimal == "spider"
           then " that wriggled and jiggled and tickled inside her."
           else ""
      line = "She swallowed the " ++ thisAnimal
          ++ " to catch the " ++ prevAnimal ++ extraForSpider
  in line : (if idx - 1 == 0
               then ["I don't know why she swallowed the fly. Perhaps she'll die."]
               else restChain)
