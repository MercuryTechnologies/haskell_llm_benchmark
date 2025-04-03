module ProteinTranslation (proteins) where

-- | Translates RNA sequences into proteins according to preset codon mappings.
-- | Stops translation upon encountering a STOP codon.
proteins :: String -> Maybe [String]
proteins rna =
  let codons = chunkInThrees rna
  in go codons []
  where
    go :: [String] -> [String] -> Maybe [String]
    go [] acc = Just (reverse acc)
    go (c:cs) acc =
      case codonToProtein c of
        Nothing       -> Nothing
        Just "STOP"   -> Just (reverse acc)
        Just protein  -> go cs (protein : acc)

    codonToProtein :: String -> Maybe String
    codonToProtein c = case c of
      "AUG"                 -> Just "Methionine"
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

    chunkInThrees :: String -> [String]
    chunkInThrees [] = []
    chunkInThrees xs =
      let (codon, rest) = splitAt 3 xs
      in if length codon < 3
           then []
           else codon : chunkInThrees rest
