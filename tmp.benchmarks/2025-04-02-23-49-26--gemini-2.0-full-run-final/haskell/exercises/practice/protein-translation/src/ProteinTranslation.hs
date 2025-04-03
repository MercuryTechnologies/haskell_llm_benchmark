module ProteinTranslation (proteins) where

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
  "UAA" -> Nothing
  "UAG" -> Nothing
  "UGA" -> Nothing
  _     -> Nothing

proteins :: String -> Maybe [String]
proteins rna =
  let codons = map (take 3) (iterate (drop 3) rna)
      translated = map codonToProtein (takeWhile (not . null) codons)
  in  fmap catMaybes $ Just $ takeWhile (/= Nothing) translated

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs
