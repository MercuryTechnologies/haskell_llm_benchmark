module House (rhyme) where

import Data.List (intercalate)

-- Subjects/things introduced in each stanza
subjects :: [String]
subjects =
  [ "house that Jack built."
  , "malt"
  , "rat"
  , "cat"
  , "dog"
  , "cow with the crumpled horn"
  , "maiden all forlorn"
  , "man all tattered and torn"
  , "priest all shaven and shorn"
  , "rooster that crowed in the morn"
  , "farmer sowing his corn"
  , "horse and the hound and the horn"
  ]

-- Actions describing each new subject, to be stacked recursively
actions :: [String]
actions =
  [ ""  -- placeholder for the first item, which needs no "that" line
  , "that lay in the house that Jack built."
  , "that ate the malt"
  , "that killed the rat"
  , "that worried the cat"
  , "that tossed the dog"
  , "that milked the cow with the crumpled horn"
  , "that kissed the maiden all forlorn"
  , "that married the man all tattered and torn"
  , "that woke the priest all shaven and shorn"
  , "that kept the rooster that crowed in the morn"
  , "that belonged to the farmer sowing his corn"
  ]

-- Build all lines of a single stanza
buildStanza :: Int -> [String]
buildStanza i =
  let firstLine = "This is the " ++ subjects !! i
      restLines = map (actions !!) (reverse [1..i])
      filteredLines = filter (not . null) restLines
  in firstLine : filteredLines

rhyme :: String
rhyme =
  unlines $
    concatMap buildStanza [0 .. length subjects - 1]
