module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right ""
toRNA (x:xs) =
  case mapNucleotide x of
    Nothing -> Left x
    Just y ->
      case toRNA xs of
        Left invalid -> Left invalid
        Right rest -> Right (y : rest)

mapNucleotide :: Char -> Maybe Char
mapNucleotide 'G' = Just 'C'
mapNucleotide 'C' = Just 'G'
mapNucleotide 'T' = Just 'A'
mapNucleotide 'A' = Just 'U'
mapNucleotide _   = Nothing
