module ProteinTranslation(proteins) where

import Data.Maybe ( catMaybes, isJust )

-- | Translates a single codon into a Maybe String.
-- Returns Just "AminoAcid" for a valid amino acid codon,
-- Nothing for a STOP codon.
translate :: String -> Maybe String
translate "AUG" = Just "Methionine"
translate "UUU" = Just "Phenylalanine"
translate "UUC" = Just "Phenylalanine"
translate "UUA" = Just "Leucine"
translate "UUG" = Just "Leucine"
translate "UCU" = Just "Serine"
translate "UCC" = Just "Serine"
translate "UCA" = Just "Serine"
translate "UCG" = Just "Serine"
translate "UAU" = Just "Tyrosine"
translate "UAC" = Just "Tyrosine"
translate "UGU" = Just "Cysteine"
translate "UGC" = Just "Cysteine"
translate "UGG" = Just "Tryptophan"
translate "UAA" = Nothing -- STOP
translate "UAG" = Nothing -- STOP
translate "UGA" = Nothing -- STOP
translate _     = Nothing -- Treat any other codon as STOP/invalid

-- | Splits an RNA string into a list of 3-character codons.
-- Ignores any trailing characters if the length is not a multiple of 3.
toCodons :: String -> [String]
toCodons [] = []
toCodons rna
  | length rna < 3 = []
  | otherwise      = take 3 rna : toCodons (drop 3 rna)

-- | Translates an RNA sequence into a list of proteins.
-- Translation stops at the first STOP codon encountered.
proteins :: String -> Maybe [String]
proteins rna = Just . catMaybes . takeWhile isJust . map translate $ toCodons rna
