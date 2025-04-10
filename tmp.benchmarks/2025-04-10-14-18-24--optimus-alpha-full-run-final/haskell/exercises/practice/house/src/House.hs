module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n\n" $ map verse [1..length pieces]

verse :: Int -> String
verse n = intercalate "\n" ("This is " : map ("  " ++) (verseLines n))

verseLines :: Int -> [String]
verseLines n = [subjects !! (n-1)] ++ map (\i -> actions !! i ++ " " ++ subjects !! i) [n-2, n-3 .. 0]

subjects :: [String]
subjects =
  [ "the house that Jack built."
  , "the malt"
  , "the rat"
  , "the cat"
  , "the dog"
  , "the cow with the crumpled horn"
  , "the maiden all forlorn"
  , "the man all tattered and torn"
  , "the priest all shaven and shorn"
  , "the rooster that crowed in the morn"
  , "the farmer sowing his corn"
  , "the horse and the hound and the horn"
  ]

actions :: [String]
actions =
  [ "that lay in"
  , "that ate"
  , "that killed"
  , "that worried"
  , "that tossed"
  , "that milked"
  , "that kissed"
  , "that married"
  , "that woke"
  , "that kept"
  , "that belonged to"
  ]
