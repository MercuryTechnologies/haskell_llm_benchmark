module Frequency (frequency) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies (parMap, rseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let processed = parMap rseq (countLetters . T.unpack) texts
    in Map.unionsWith (+) processed

countLetters :: String -> Map Char Int
countLetters = foldr (\c -> Map.insertWith (+) (toLower c) 1) Map.empty
    where toLower c
            | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
            | otherwise = c
