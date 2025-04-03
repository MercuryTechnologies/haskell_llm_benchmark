module Frequency (frequency) where

import Data.Map  (Map, fromList, insertWith)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Char (toLower, isLetter)
import Control.Parallel.Strategies (parMap, rseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  foldl combineMaps mempty $ parMap rseq countLetters texts
  where
    countLetters text =
      fromList
        [ (toLower c, 1)
        | c <- unpack text, isLetter c
        ]
    combineMaps m1 m2 =
      foldl (\acc (c, count) -> insertWith (+) c count acc) m1 $ Data.Map.fromList [(c, Data.Map.findWithDefault 0 c m2) | c <- Data.Map.keys m2]
    mempty = fromList []
