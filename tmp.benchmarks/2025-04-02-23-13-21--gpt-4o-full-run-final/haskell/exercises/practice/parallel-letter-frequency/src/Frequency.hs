module Frequency (frequency) where

import Data.Map (Map, fromListWith, toList)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Char (toLower, isLetter)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let chunks = chunk nWorkers texts
        freqs = map countLetters chunks `using` parList rdeepseq
    in fromListWith (+) (concat freqs)

chunk :: Int -> [a] -> [[a]]
chunk n xs = go n xs
  where
    go _ [] = []
    go 0 xs = [xs]
    go n xs = let (h, t) = splitAt (length xs `div` n) xs
              in h : go (n - 1) t

countLetters :: [Text] -> [(Char, Int)]
countLetters texts = 
    [ (c', 1) 
    | text <- texts
    , c <- T.unpack text
    , isLetter c
    , let c' = toLower c
    ]
