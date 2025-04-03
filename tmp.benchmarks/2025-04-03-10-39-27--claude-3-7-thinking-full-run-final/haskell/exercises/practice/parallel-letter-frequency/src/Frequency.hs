module Frequency (frequency) where

import Data.Map      (Map)
import qualified Data.Map as Map
import Data.Text     (Text)
import qualified Data.Text as Text
import Data.Char     (isLetter, toLower)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, replicateM)

-- | Count the frequency of letters in a list of texts using parallelism.
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | null texts = Map.empty
  | otherwise  = unsafePerformIO $ runInParallel nWorkers countLetters (chunk nWorkers texts)
  where
    countLetters :: [Text] -> Map Char Int
    countLetters = foldr (Map.unionWith (+) . countText) Map.empty

    countText :: Text -> Map Char Int
    countText text = Map.fromListWith (+) [(toLower c, 1) | c <- Text.unpack text, isLetter c]

    -- Divide the list into roughly equal chunks
    chunk :: Int -> [a] -> [[a]]
    chunk n xs
      | n <= 0    = [xs]
      | null xs   = []
      | otherwise =
          let (as, bs) = splitAt (length xs `div` max 1 n) xs
          in as : chunk (n - 1) bs

    -- Run a function on each chunk in parallel and collect results
    runInParallel :: Int -> ([a] -> b) -> [[a]] -> IO b
    runInParallel n f chunks = do
      let actualChunks = take n $ filter (not . null) chunks
      results <- replicateM (length actualChunks) newEmptyMVar
      
      -- Spawn a thread for each chunk
      forM_ (zip actualChunks results) $ \(chunk', resultVar) ->
        forkIO $ putMVar resultVar (f chunk')
      
      -- Collect results
      maps <- mapM takeMVar results
      return $ foldr Map.unionWith Map.empty maps

-- For use in runInParallel
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar (MVar)
import Control.Monad (forM_)
