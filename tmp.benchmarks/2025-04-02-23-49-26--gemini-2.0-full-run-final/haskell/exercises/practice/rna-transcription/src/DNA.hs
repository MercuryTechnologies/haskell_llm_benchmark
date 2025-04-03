module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = foldl processNucleotide (Right "") xs
  where
    processNucleotide :: Either Char String -> Char -> Either Char String
    processNucleotide acc dnaNucleotide =
      case (acc, dnaNucleotide) of
        (Right rna, 'G') -> Right ('C' : rna)
        (Right rna, 'C') -> Right ('G' : rna)
        (Right rna, 'T') -> Right ('A' : rna)
        (Right rna, 'A') -> Right ('U' : rna)
        (_, invalid) -> Left invalid
