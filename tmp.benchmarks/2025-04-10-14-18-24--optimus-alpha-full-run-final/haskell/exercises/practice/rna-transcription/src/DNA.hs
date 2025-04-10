module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = go []
  where
    go acc [] = Right (reverse acc)
    go acc (x:xs) =
      case complement x of
        Just c  -> go (c:acc) xs
        Nothing -> Left x

    complement 'G' = Just 'C'
    complement 'C' = Just 'G'
    complement 'T' = Just 'A'
    complement 'A' = Just 'U'
    complement _   = Nothing
