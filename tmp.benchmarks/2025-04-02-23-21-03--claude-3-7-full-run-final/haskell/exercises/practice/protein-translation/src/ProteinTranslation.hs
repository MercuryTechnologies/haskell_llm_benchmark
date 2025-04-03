module ProteinTranslation(proteins) where

-- | Translates RNA sequences into proteins
proteins :: String -> Maybe [String]
proteins rna = Just (translateCodons (chunksOf 3 rna))

-- | Splits a string into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Translates a list of codons into proteins until a STOP codon is encountered
translateCodons :: [String] -> [String]
translateCodons [] = []
translateCodons (codon:rest)
  | length codon < 3 = []  -- Ignore incomplete codons
  | otherwise = case translateCodon codon of
      Just "STOP" -> []  -- Stop translation when encountering a STOP codon
      Just protein -> protein : translateCodons rest
      Nothing -> []  -- Invalid codon, stop translation

-- | Translates a single codon to its corresponding protein
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
  _     -> Nothing  -- Unknown codon
