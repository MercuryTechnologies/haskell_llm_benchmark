module ProteinTranslation (proteins) where

-- Break an RNA string into codons of length 3
chunk3 :: String -> [String]
chunk3 [] = []
chunk3 xs
  | length xs < 3 = []
  | otherwise =
      let (codon, rest) = splitAt 3 xs
      in codon : chunk3 rest

-- Translate one codon into the corresponding protein or "STOP"
codonToProtein :: String -> Maybe String
codonToProtein c
  | c `elem` ["UAA","UAG","UGA"] = Just "STOP"
  | c == "AUG"                   = Just "Methionine"
  | c `elem` ["UUU","UUC"]       = Just "Phenylalanine"
  | c `elem` ["UUA","UUG"]       = Just "Leucine"
  | c `elem` ["UCU","UCC","UCA","UCG"] = Just "Serine"
  | c `elem` ["UAU","UAC"]       = Just "Tyrosine"
  | c `elem` ["UGU","UGC"]       = Just "Cysteine"
  | c == "UGG"                   = Just "Tryptophan"
  | otherwise                    = Nothing

-- Helper to recursively process codons, stopping on STOP or unrecognized codons
translateCodons :: [String] -> Maybe [String]
translateCodons [] = Just []
translateCodons (c:cs) =
  case codonToProtein c of
    Nothing        -> Nothing          -- Unrecognized codon
    Just "STOP"    -> Just []          -- Stop codon -> end translation
    Just protein   -> do
      rest <- translateCodons cs
      Just (protein : rest)

-- Main function to translate an RNA string into a list of proteins
proteins :: String -> Maybe [String]
proteins rna = translateCodons (chunk3 rna)
