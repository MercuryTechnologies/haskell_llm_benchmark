module DNA (toRNA) where

import Data.Traversable (traverse)

-- | Transcribes a single DNA nucleotide to its RNA complement.
-- Returns Left with the invalid character if it's not a valid DNA nucleotide.
dnaToRnaNucleotide :: Char -> Either Char Char
dnaToRnaNucleotide 'G' = Right 'C'
dnaToRnaNucleotide 'C' = Right 'G'
dnaToRnaNucleotide 'T' = Right 'A'
dnaToRnaNucleotide 'A' = Right 'U'
dnaToRnaNucleotide invalid = Left invalid

-- | Transcribes a DNA strand to its RNA complement.
-- If the input string contains invalid nucleotides, returns Left with the first invalid character.
-- Otherwise, returns Right with the transcribed RNA string.
toRNA :: String -> Either Char String
toRNA = traverse dnaToRnaNucleotide
