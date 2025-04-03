module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = translateCodons (chunksOf 3 rna)
  where
    -- Split a list into chunks of size n
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs
      | length xs < n = [xs] -- Include incomplete chunks
      | otherwise = take n xs : chunksOf n (drop n xs)
    
    -- Translate codons into proteins, stopping at a STOP codon
    translateCodons :: [String] -> Maybe [String]
    translateCodons [] = Just []
    translateCodons (c:cs)
      | length c < 3 = Nothing -- Incomplete codon
      | c `elem` ["UAA", "UAG", "UGA"] = Just [] -- Stop translation
      | otherwise = case codonToProtein c of
          Just protein -> do
            rest <- translateCodons cs
            return (protein : rest)
          Nothing -> Nothing
    
    -- Map a codon to a protein
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
    codonToProtein _ = Nothing
