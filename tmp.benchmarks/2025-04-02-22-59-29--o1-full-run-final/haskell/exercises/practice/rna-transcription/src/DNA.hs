module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = traverse complement xs
  where
    complement 'G' = Right 'C'
    complement 'C' = Right 'G'
    complement 'T' = Right 'A'
    complement 'A' = Right 'U'
    complement c   = Left c
