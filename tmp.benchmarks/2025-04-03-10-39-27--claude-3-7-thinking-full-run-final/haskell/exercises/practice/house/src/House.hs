module House (rhyme) where

import Data.List (intercalate)

-- The subjects in the rhyme
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

-- The verbs in the rhyme
verbs :: [String]
verbs =
  [ ""  -- Placeholder (verb 0 is never used)
  , "lay in"
  , "ate"
  , "killed"
  , "worried"
  , "tossed"
  , "milked"
  , "kissed"
  , "married"
  , "woke"
  , "kept"
  , "belonged to"
  ]

-- Build a line describing what subject i does to subject i-1
buildLine :: Int -> String
buildLine i = "that " ++ verbs !! i ++ " the " ++ subjects !! (i-1)

-- Build a verse for a given index
verse :: Int -> String
verse 0 = "This is the " ++ subjects !! 0 ++ "."
verse n = "This is the " ++ subjects !! n ++ "\n" ++ 
          intercalate "\n" [buildLine i | i <- [n,n-1..1]] ++ "."

-- Generate the complete rhyme
rhyme :: String
rhyme = intercalate "\n\n" [verse i | i <- [0..11]]
