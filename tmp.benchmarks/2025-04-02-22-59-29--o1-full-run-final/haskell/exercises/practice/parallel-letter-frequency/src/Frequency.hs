module Frequency (frequency) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM, mapM)
import Data.Char (isAlpha, toLower)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- | Count the frequency of letters in the given list of texts using
--   the specified number of workers in parallel.
--   Returns a Map Char Int, where each Char is in lowercase
--   and only alphabetic characters are counted.
--------------------------------------------------------------------------------
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 1 = partialFrequency texts
  | otherwise = unsafePerformIO $ do
      let chunks = splitListInto nWorkers texts
      vars <- forM chunks $ \c -> do
        mv <- newEmptyMVar
        _ <- forkIO $ putMVar mv (partialFrequency c)
        return mv
      results <- mapM takeMVar vars
      return (foldl' (Map.unionWith (+)) Map.empty results)

--------------------------------------------------------------------------------
-- | Count frequencies for a slice of the texts.
--------------------------------------------------------------------------------
partialFrequency :: [Text] -> Map Char Int
partialFrequency =
  foldl' (Map.unionWith (+)) Map.empty . map countText
  where
    countText t =
      T.foldl'
        (\acc c ->
           if isAlpha c
             then Map.insertWith (+) (toLower c) 1 acc
             else acc
        )
        Map.empty
        t

--------------------------------------------------------------------------------
-- | Split a list into at most n parts of roughly equal size.
--------------------------------------------------------------------------------
splitListInto :: Int -> [a] -> [[a]]
splitListInto n xs =
  let len = length xs
      (q, r) = len `quotRem` n
      sizes = replicate r (q + 1) ++ replicate (n - r) q
   in go xs sizes
  where
    go _  []     = []
    go ys (s:ss) =
      let (h, t) = splitAt s ys
       in h : go t ss
