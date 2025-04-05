module ProteinTranslation(proteins) where

codonToProtein :: String -> Maybe String
codonToProtein codon = case codon of
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

splitCodons :: String -> [String]
splitCodons [] = []
splitCodons xs
    | length xs < 3 = []
    | otherwise = let (codon, rest) = splitAt 3 xs
                  in codon : splitCodons rest

proteins :: String -> Maybe [String]
proteins rna = go (splitCodons rna) []
  where
    go [] acc = Just (reverse acc)
    go (c:cs) acc = case codonToProtein c of
        Nothing -> Nothing
        Just "STOP" -> Just (reverse acc)
        Just protein -> go cs (protein : acc)
