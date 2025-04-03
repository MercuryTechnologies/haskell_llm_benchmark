module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = foldl transcribe (Right "") xs
  where
    transcribe (Right acc) 'G' = Right (acc ++ "C")
    transcribe (Right acc) 'C' = Right (acc ++ "G")
    transcribe (Right acc) 'T' = Right (acc ++ "A")
    transcribe (Right acc) 'A' = Right (acc ++ "U")
    transcribe _ invalid = Left invalid
