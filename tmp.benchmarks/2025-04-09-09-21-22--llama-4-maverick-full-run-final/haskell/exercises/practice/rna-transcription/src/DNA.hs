module DNA (toRNA) where

import Data.Char (toUpper)

toRNA :: String -> Either Char String
toRNA = mapLeftM (transcribe . toUpper)
  where
    transcribe 'G' = Right 'C'
    transcribe 'C' = Right 'G'
    transcribe 'T' = Right 'A'
    transcribe 'A' = Right 'U'
    transcribe c   = Left c

    mapLeftM _ []     = Right []
    mapLeftM f (x:xs) = case f x of
                          Right y  -> fmap (y:) (mapLeftM f xs)
                          Left err -> Left err
