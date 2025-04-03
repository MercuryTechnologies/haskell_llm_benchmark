module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
    let initialMap = M.fromList [(A,0), (C,0), (G,0), (T,0)]
    in foldr
       (\ch acc ->
           case acc of
             Left err -> Left err
             Right m ->
               case charToNucleotide ch of
                 Nothing -> Left "Invalid nucleotide found"
                 Just n  -> Right (M.adjust (+1) n m)
       )
       (Right initialMap)
       xs

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide c = case c of
  'A' -> Just A
  'C' -> Just C
  'G' -> Just G
  'T' -> Just T
  _   -> Nothing
