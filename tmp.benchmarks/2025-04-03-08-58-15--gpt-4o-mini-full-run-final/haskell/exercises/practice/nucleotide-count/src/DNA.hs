module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromListWith, toList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | any (`notElem` "ACGT") xs = Left "Invalid nucleotide found"
    | otherwise = Right $ fromListWith (+) [(nucleotideFromChar x, 1) | x <- xs]

nucleotideFromChar :: Char -> Nucleotide
nucleotideFromChar 'A' = A
nucleotideFromChar 'C' = C
nucleotideFromChar 'G' = G
nucleotideFromChar 'T' = T
