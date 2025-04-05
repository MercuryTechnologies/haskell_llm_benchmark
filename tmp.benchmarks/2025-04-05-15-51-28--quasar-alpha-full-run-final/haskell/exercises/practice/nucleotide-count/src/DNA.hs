module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    if all validChar xs then
        Right $ foldr countNuc initialMap xs
    else
        Left "Invalid DNA sequence"
  where
    validChar c = c `elem` "ACGT"
    initialMap = Map.fromList [(A,0), (C,0), (G,0), (T,0)]
    countNuc c m = case charToNuc c of
                     Just nuc -> Map.adjust (+1) nuc m
                     Nothing  -> m  -- This case won't occur due to validation

    charToNuc 'A' = Just A
    charToNuc 'C' = Just C
    charToNuc 'G' = Just G
    charToNuc 'T' = Just T
    charToNuc _   = Nothing
