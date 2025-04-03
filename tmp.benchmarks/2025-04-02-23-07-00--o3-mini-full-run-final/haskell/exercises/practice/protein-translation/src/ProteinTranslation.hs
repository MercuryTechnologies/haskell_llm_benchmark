module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna
  | null rna = Just []
  | length rna `mod` 3 /= 0 = Nothing
  | otherwise = processCodons (chunk rna)

chunk :: String -> [String]
chunk [] = []
chunk s = let (c, rest) = splitAt 3 s in c : chunk rest

processCodons :: [String] -> Maybe [String]
processCodons [] = Just []
processCodons (c:cs) =
  case translateCodon c of
    Just "STOP" -> Just []
    Just protein -> do
      proteinsRest <- processCodons cs
      Just (protein : proteinsRest)
    Nothing -> Nothing

translateCodon :: String -> Maybe String
translateCodon "AUG" = Just "Methionine"
translateCodon "UUU" = Just "Phenylalanine"
translateCodon "UUC" = Just "Phenylalanine"
translateCodon "UUA" = Just "Leucine"
translateCodon "UUG" = Just "Leucine"
translateCodon "UCU" = Just "Serine"
translateCodon "UCC" = Just "Serine"
translateCodon "UCA" = Just "Serine"
translateCodon "UCG" = Just "Serine"
translateCodon "UAU" = Just "Tyrosine"
translateCodon "UAC" = Just "Tyrosine"
translateCodon "UGU" = Just "Cysteine"
translateCodon "UGC" = Just "Cysteine"
translateCodon "UGG" = Just "Tryptophan"
translateCodon "UAA" = Just "STOP"
translateCodon "UAG" = Just "STOP"
translateCodon "UGA" = Just "STOP"
translateCodon _     = Nothing
