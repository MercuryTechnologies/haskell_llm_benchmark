module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide 'A' = Just A
toNucleotide 'C' = Just C
toNucleotide 'G' = Just G
toNucleotide 'T' = Just T
toNucleotide _   = Nothing

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
  case mapM toNucleotide xs of
    Nothing -> Left "Invalid nucleotide"
    Just ns -> Right $ fromListWith (+) (map (\n -> (n, 1)) ns)
