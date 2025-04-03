module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- | Counts the number of occurrences of each nucleotide in a DNA strand.
--
-- Returns an error ('Left' String) if the strand contains invalid nucleotides.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldr processChar (Right initialMap) xs
  where
    -- Initial map with all nucleotides set to 0 count.
    initialMap :: Map Nucleotide Int
    initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    -- | Processes a single character, updating the count map or returning an error.
    processChar :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
    processChar _ (Left err) = Left err -- Propagate existing error
    processChar c (Right currentMap) =
      case c of
        'A' -> Right $ Map.adjust (+1) A currentMap
        'C' -> Right $ Map.adjust (+1) C currentMap
        'G' -> Right $ Map.adjust (+1) G currentMap
        'T' -> Right $ Map.adjust (+1) T currentMap
        _   -> Left $ "Invalid nucleotide in strand: " ++ [c] -- Found an invalid character
