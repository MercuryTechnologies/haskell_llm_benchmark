module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    case validateDNA xs of
        Left err -> Left err
        Right _ -> Right $ countNucleotides xs

validateDNA :: String -> Either String ()
validateDNA = mapM_ validateChar
  where
    validateChar c = case c of
        'A' -> Right ()
        'C' -> Right ()
        'G' -> Right ()
        'T' -> Right ()
        _   -> Left ("Invalid nucleotide '" ++ [c] ++ "'")

countNucleotides :: String -> Map Nucleotide Int
countNucleotides = foldl' countChar initialCounts
  where
    initialCounts = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    countChar counts c = Map.adjust (+1) (toNucleotide c) counts
    toNucleotide 'A' = A
    toNucleotide 'C' = C
    toNucleotide 'G' = G
    toNucleotide 'T' = T
    toNucleotide _   = error "Should never happen - already validated"
