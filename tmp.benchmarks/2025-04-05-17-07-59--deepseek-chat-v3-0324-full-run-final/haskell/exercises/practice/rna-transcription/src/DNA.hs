module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM dnaToRna
  where
    dnaToRna 'G' = Right 'C'
    dnaToRna 'C' = Right 'G'
    dnaToRna 'T' = Right 'A'
    dnaToRna 'A' = Right 'U'
    dnaToRna c   = Left c
