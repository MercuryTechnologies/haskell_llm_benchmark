module ProteinTranslation(proteins) where

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)

-- Function to translate RNA sequence into proteins
proteins :: String -> Maybe [String]
proteins rna = Just $ takeWhile (/= "STOP") $ mapMaybe translateCodon codons
  where
    codons = unfoldr (\s -> if null s then Nothing else Just (splitAt 3 s)) rna

-- Function to translate a single codon into a protein
translateCodon :: String -> Maybe String
translateCodon codon = case codon of
    "AUG" -> Just "Methionine"
    "UUU" -> Just "Phenylalanine"
    "UUC" -> Just "Phenylalanine"
    "UUA" -> Just "Leucine"
    "UUG" -> Just "Leucine"
    "UCU" -> Just "Serine"
    "UCC" -> Just "Serine"
    "UCA" -> Just "Serine"
    "UCG" -> Just "Serine"
    "UAU" -> Just "Tyrosine"
    "UAC" -> Just "Tyrosine"
    "UGU" -> Just "Cysteine"
    "UGC" -> Just "Cysteine"
    "UGG" -> Just "Tryptophan"
    "UAA" -> Just "STOP"
    "UAG" -> Just "STOP"
    "UGA" -> Just "STOP"
    _     -> Nothing
