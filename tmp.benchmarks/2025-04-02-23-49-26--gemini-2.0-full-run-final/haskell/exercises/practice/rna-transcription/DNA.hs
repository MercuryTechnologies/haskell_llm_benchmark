module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = foldr processNucleotide (Right "")
  where
    processNucleotide :: Char -> Either Char String -> Either Char String
    processNucleotide dnaNucleotide acc =
      case (dnaNucleotide, acc) of
        ('G', Right rna) -> Right ('C' : rna)
        ('C', Right rna) -> Right ('G' : rna)
        ('T', Right rna) -> Right ('A' : rna)
        ('A', Right rna) -> Right ('U' : rna)
        (invalid, _) -> Left invalid
