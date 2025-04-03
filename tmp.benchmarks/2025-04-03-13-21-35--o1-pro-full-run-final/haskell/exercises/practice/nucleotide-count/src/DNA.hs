module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
    let initialMap = M.fromList [(A,0),(C,0),(G,0),(T,0)]
        go m 'A' = Right (M.adjust (+1) A m)
        go m 'C' = Right (M.adjust (+1) C m)
        go m 'G' = Right (M.adjust (+1) G m)
        go m 'T' = Right (M.adjust (+1) T m)
        go _ _   = Left "Invalid nucleotide"
    in foldl
         (\acc c -> case acc of
                      Left e -> Left e
                      Right m' -> go m' c
         )
         (Right initialMap)
         xs
