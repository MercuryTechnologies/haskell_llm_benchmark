module Frequency (frequency) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isLetter, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M -- Use strict map for performance
import Data.Text (Text)
import qualified Data.Text as T

-- | Count the frequency of letters within a single Text.
-- Only counts ASCII letters, case-insensitively.
countChars :: Text -> Map Char Int
countChars text = T.foldl' countChar M.empty text
  where
    countChar :: Map Char Int -> Char -> Map Char Int
    countChar acc char
      | isLetter char = M.insertWith (+) (toLower char) 1 acc
      | otherwise     = acc

-- | Calculate letter frequency in parallel.
-- The nWorkers argument is not directly used by this implementation strategy;
-- parallelism is managed by the GHC runtime system (+RTS -N).
frequency :: Int -> [Text] -> Map Char Int
frequency _nWorkers texts = M.unionsWith (+) (parMap rpar countChars texts)

