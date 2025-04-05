module ProteinTranslation(proteins) where

import Data.Maybe (mapMaybe)

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

splitCodons :: String -> [String]
splitCodons [] = []
splitCodons rna = take 3 rna : splitCodons (drop 3 rna)

proteins :: String -> Maybe [String]
proteins rna = 
    let codons = splitCodons rna
        translations = map codonToProtein codons
        proteinsList = takeWhileJust translations
    in Just proteinsList
  where
    takeWhileJust :: [Maybe a] -> [a]
    takeWhileJust [] = []
    takeWhileJust (Just x : xs) = x : takeWhileJust xs
    takeWhileJust (Nothing : _) = []
