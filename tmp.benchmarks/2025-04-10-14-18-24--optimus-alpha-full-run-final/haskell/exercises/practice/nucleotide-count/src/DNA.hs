module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    if all isValid xs
        then Right $ foldr countNuc initialMap xs
        else Left "Invalid nucleotide found"
  where
    initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    isValid c = c `elem` "ACGT"
    countNuc c m = case charToNuc c of
        Just n  -> Map.adjust (+1) n m
        Nothing -> m -- This case won't occur due to isValid check
    charToNuc 'A' = Just A
    charToNuc 'C' = Just C
    charToNuc 'G' = Just G
    charToNuc 'T' = Just T
    charToNuc _   = Nothing
