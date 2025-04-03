module Frequency (frequency) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Data.Char (isLetter, toLower)
import Data.List (foldl')
import System.IO.Unsafe (unsafePerformIO)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 1 = foldl' combine Map.empty (map countFrequency texts)
  | otherwise =
      let actualWorkers = min nWorkers (length texts)
          chunks = splitInto actualWorkers texts
          maps = parallelCompute chunks
      in mergeMaps maps

countFrequency :: Text -> Map Char Int
countFrequency txt =
  foldl' (\acc c -> if isLetter c then Map.insertWith (+) (toLower c) 1 acc else acc)
         Map.empty
         (unpack txt)

combine :: Map Char Int -> Map Char Int -> Map Char Int
combine = Map.unionWith (+)

splitInto :: Int -> [a] -> [[a]]
splitInto n xs = go xs n
  where
    go [] _ = []
    go _ 0 = []
    go ys m =
      let chunkSize = (length ys + m - 1) `div` m
          (chunk, rest) = splitAt chunkSize ys
      in chunk : go rest (m - 1)

parallelCompute :: [[Text]] -> [Map Char Int]
parallelCompute chunks = unsafePerformIO $ do
  mvars <- forM chunks $ \chunk -> do
    mvar <- newEmptyMVar
    _ <- forkIO $ putMVar mvar (foldl' combine Map.empty (map countFrequency chunk))
    return mvar
  forM mvars takeMVar

mergeMaps :: [Map Char Int] -> Map Char Int
mergeMaps = foldl' combine Map.empty
