module House (rhyme) where

import Data.List (intercalate)

subjects :: [String]
subjects =
  [ "house that Jack built"
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

actions :: [String]
actions =
  [ "that lay in the house that Jack built"
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

-- | Generate a single verse given an index (0-based).
--   The verse starts with "This is" + the subject at the given index.
--   Then, for verses beyond the first one, it appends the linking actions
--   from the previous subjects in reverse order.
--   Only the final line in a verse ends with a period.
verse :: Int -> String
verse i =
  let primary = "This is the " ++ (subjects !! i)
      extraLines = [actions !! j | j <- reverse [0 .. i - 1]]
      allLines = case primary : extraLines of
        [] -> []  -- This case won't occur.
        ls -> init ls ++ [last ls ++ "."]
  in unlines allLines

-- | The full nursery rhyme constructed as cumulative verses.
--   Verses are separated by a blank line.
rhyme :: String
rhyme =
  let verses = [verse i | i <- [0 .. length subjects - 1]]
  in intercalate "\n" verses
