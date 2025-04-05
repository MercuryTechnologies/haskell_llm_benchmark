module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = transcribe
  where
    transcribe [] = Right []
    transcribe (x:xs) =
      case nucleotide x of
        Just rnaBase -> 
          case transcribe xs of
            Right rest -> Right (rnaBase : rest)
            Left err -> Left err
        Nothing -> Left x

    nucleotide 'G' = Just 'C'
    nucleotide 'C' = Just 'G'
    nucleotide 'T' = Just 'A'
    nucleotide 'A' = Just 'U'
    nucleotide _   = Nothing
