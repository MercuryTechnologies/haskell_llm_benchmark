module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, insertWith)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  foldr processNucleotide (Right Map.empty) xs
  where
    processNucleotide :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
    processNucleotide nucleotide acc = do
      counts <- acc
      case nucleotide of
        'A' -> Right $ insertWith (+) A 1 counts
        'C' -> Right $ insertWith (+) C 1 counts
        'G' -> Right $ insertWith (+) G 1 counts
        'T' -> Right $ insertWith (+) T 1 counts
        _   -> Left $ "Invalid nucleotide '" ++ [nucleotide] ++ "'"
