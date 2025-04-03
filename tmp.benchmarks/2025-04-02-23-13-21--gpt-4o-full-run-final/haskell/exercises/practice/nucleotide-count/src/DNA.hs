module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, insertWith, empty)
import Data.Either (lefts, rights)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | not (null invalids) = Left "error"
  | otherwise = Right $ foldr countNucleotide empty validNucleotides
  where
    (invalids, validNucleotides) = partitionEithers $ map toNucleotide xs
    toNucleotide 'A' = Right A
    toNucleotide 'C' = Right C
    toNucleotide 'G' = Right G
    toNucleotide 'T' = Right T
    toNucleotide _   = Left "Invalid nucleotide"
    countNucleotide n = insertWith (+) n 1

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a (l, r) = (a:l, r)
    right b (l, r) = (l, b:r)
