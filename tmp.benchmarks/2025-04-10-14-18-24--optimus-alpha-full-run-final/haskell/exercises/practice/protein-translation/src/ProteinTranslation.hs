module ProteinTranslation(proteins) where

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)

-- Map a codon to its corresponding protein, or Nothing for STOP codons or invalid codons
codonToProtein :: String -> Maybe (Maybe String)
codonToProtein codon = case codon of
    "AUG"       -> Just (Just "Methionine")
    "UUU"       -> Just (Just "Phenylalanine")
    "UUC"       -> Just (Just "Phenylalanine")
    "UUA"       -> Just (Just "Leucine")
    "UUG"       -> Just (Just "Leucine")
    "UCU"       -> Just (Just "Serine")
    "UCC"       -> Just (Just "Serine")
    "UCA"       -> Just (Just "Serine")
    "UCG"       -> Just (Just "Serine")
    "UAU"       -> Just (Just "Tyrosine")
    "UAC"       -> Just (Just "Tyrosine")
    "UGU"       -> Just (Just "Cysteine")
    "UGC"       -> Just (Just "Cysteine")
    "UGG"       -> Just (Just "Tryptophan")
    "UAA"       -> Just Nothing -- STOP
    "UAG"       -> Just Nothing -- STOP
    "UGA"       -> Just Nothing -- STOP
    _           -> Nothing      -- Invalid codon

-- Split the RNA string into codons (triplets)
toCodons :: String -> [String]
toCodons [] = []
toCodons xs
    | length xs < 3 = []
    | otherwise = take 3 xs : toCodons (drop 3 xs)

-- Main function: translate RNA string to list of proteins, or Nothing if invalid codon
proteins :: String -> Maybe [String]
proteins rna = go (toCodons rna)
  where
    go [] = Just []
    go (c:cs) = case codonToProtein c of
        Nothing -> Nothing -- Invalid codon
        Just Nothing -> Just [] -- STOP codon
        Just (Just protein) -> case go cs of
            Nothing -> Nothing
            Just rest -> Just (protein : rest)
