module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = foldr convert (Right "")
  where
    convert nucleotide acc = case (nucleotide, acc) of
      (_, Left err) -> Left err  -- Propagate error
      ('G', Right rna) -> Right ('C' : rna)
      ('C', Right rna) -> Right ('G' : rna)
      ('T', Right rna) -> Right ('A' : rna)
      ('A', Right rna) -> Right ('U' : rna)
      (invalid, _) -> Left invalid  -- Return first invalid character
