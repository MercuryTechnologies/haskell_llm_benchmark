module House (rhyme) where

import Data.List (intercalate)

-- Each tuple represents a part of the rhyme:
-- (Subject, Verb phrase connecting this subject to the *previous* subject)
parts :: [(String, String)]
parts =
  [ ("the house that Jack built.", "") -- Base case, verb phrase is not used for connection
  , ("the malt", "that lay in")
  , ("the rat", "that ate")
  , ("the cat", "that killed")
  , ("the dog", "that worried")
  , ("the cow with the crumpled horn", "that tossed")
  , ("the maiden all forlorn", "that milked")
  , ("the man all tattered and torn", "that kissed")
  , ("the priest all shaven and shorn", "that married")
  , ("the rooster that crowed in the morn", "that woke")
  , ("the farmer sowing his corn", "that kept")
  , ("the horse and the hound and the horn", "that belonged to")
  ]

-- | Generates the lines for a specific verse.
-- n: The verse number (1-based index).
verseLines :: Int -> [String]
verseLines n
  | n <= 0 = error "Verse index must be positive"
  -- The first line introduces the subject of the nth part.
  | otherwise = ("This is " ++ fst (parts !! (n-1))) : buildEnding (n-1)
  where
    -- | Builds the recursive ending lines of a verse.
    -- k: The 1-based index from the 'parts' list for the current line being generated.
    --    It connects part k (verb) with part k-1 (subject).
    buildEnding :: Int -> [String]
    buildEnding k
      | k <= 0 = [] -- Base case: no more lines to add.
      -- Get the verb phrase from part k and the subject from part k-1.
      | otherwise = (snd (parts !! k) ++ " " ++ fst (parts !! (k-1))) : buildEnding (k-1)

-- | Builds a single verse string by joining its lines.
-- n: The verse number (1-based index).
buildVerse :: Int -> String
buildVerse n = intercalate "\n" (verseLines n)

-- | Generates the full nursery rhyme by combining all verses.
rhyme :: String
rhyme = intercalate "\n\n" [buildVerse i | i <- [1..length parts]] ++ "\n"
