module ProteinTranslation(proteins) where

import Data.Maybe (mapMaybe)

-- Define a mapping between codons and proteins
codonToProtein :: String -> Maybe String
codonToProtein "AUG" = Just "Methionine"
codonToProtein "UUU" = Just "Phenylalanine"
codonToProtein "UUC" = Just "Phenylalanine"
codonToProtein "UUA" = Just "Leucine"
codonToProtein "UUG" = Just "Leucine"
codonToProtein "UCU" = Just "Serine"
codonToProtein "UCC" = Just "Serine"
codonToProtein "UCA" = Just "Serine"
codonToProtein "UCG" = Just "Serine"
codonToProtein "UAU" = Just "Tyrosine"
codonToProtein "UAC" = Just "Tyrosine"
codonToProtein "UGU" = Just "Cysteine"
codonToProtein "UGC" = Just "Cysteine"
codonToProtein "UGG" = Just "Tryptophan"
codonToProtein "UAA" = Nothing
codonToProtein "UAG" = Nothing
codonToProtein "UGA" = Nothing
codonToProtein _     = Nothing

-- Function to translate RNA sequence into proteins
proteins :: String -> Maybe [String]
proteins rna = sequence $ takeWhile (/= Nothing) $ map codonToProtein $ codons rna

-- Helper function to split RNA sequence into codons
codons :: String -> [String]
codons [] = []
codons rna = take 3 rna : codons (drop 3 rna)
