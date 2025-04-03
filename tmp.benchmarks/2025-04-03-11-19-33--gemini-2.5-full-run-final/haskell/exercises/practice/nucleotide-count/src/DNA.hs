module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- | Counts the number of occurrences of each nucleotide in a DNA strand.
--
-- Returns an error ('Left' String) if the strand contains invalid nucleotides.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldl processChar initialCounts xs
  where
    -- Initial state: A map with all valid nucleotides set to 0 counts, wrapped in Right.
    initialCounts :: Either String (Map Nucleotide Int)
    initialCounts = Right $ Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    -- Function to process each character in the input string
    processChar :: Either String (Map Nucleotide Int) -> Char -> Either String (Map Nucleotide Int)
    processChar (Left err) _ = Left err -- If an error has occurred, propagate it
    processChar (Right counts) char =
      case char of
        'A' -> Right $ Map.adjust (+1) A counts
        'C' -> Right $ Map.adjust (+1) C counts
        'G' -> Right $ Map.adjust (+1) G counts
        'T' -> Right $ Map.adjust (+1) T counts
        _   -> Left $ "Invalid nucleotide in strand: " ++ [char] -- Found an invalid character
