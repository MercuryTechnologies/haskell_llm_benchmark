module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = do
  -- Initialize the map with zero counts for each nucleotide
  let initialCounts = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
  -- Process each character in the string
  foldl processChar (Right initialCounts) xs
  where
    processChar :: Either String (Map Nucleotide Int) -> Char -> Either String (Map Nucleotide Int)
    processChar (Left err) _ = Left err  -- Propagate errors
    processChar (Right counts) char = case char of
      'A' -> Right $ Map.adjust (+1) A counts
      'C' -> Right $ Map.adjust (+1) C counts
      'G' -> Right $ Map.adjust (+1) G counts
      'T' -> Right $ Map.adjust (+1) T counts
      _   -> Left $ "Invalid nucleotide: " ++ [char]
