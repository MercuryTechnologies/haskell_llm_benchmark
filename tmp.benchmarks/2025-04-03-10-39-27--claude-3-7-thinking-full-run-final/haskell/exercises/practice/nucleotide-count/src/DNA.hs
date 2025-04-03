module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    foldM updateCount initialCounts xs
  where
    initialCounts = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    
    updateCount :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
    updateCount counts c = do
      n <- charToNucleotide c
      return $ adjust (+1) n counts
      
    charToNucleotide :: Char -> Either String Nucleotide
    charToNucleotide 'A' = Right A
    charToNucleotide 'C' = Right C
    charToNucleotide 'G' = Right G
    charToNucleotide 'T' = Right T
    charToNucleotide x   = Left $ "Invalid nucleotide: " ++ [x]
