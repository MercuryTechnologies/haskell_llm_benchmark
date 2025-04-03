module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM addNucleotide Map.empty xs
  where
    addNucleotide acc c =
      case charToNucleotide c of
        Just nucleotide -> Right (Map.insertWith (+) nucleotide 1 acc)
        Nothing -> Left ("invalid nucleotide: " ++ [c])

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _   = Nothing
