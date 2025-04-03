module TwoBucket (measure) where

import Data.Maybe (fromJust)
import Data.List (find)
import qualified Data.Set as Set
import Data.Set (Set)

type State = (Int, Int, Int) -- (bucket1, bucket2, actions)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target = bfs [(cap1, 0, 1)] Set.empty
  where
    bfs :: [State] -> Set (Int, Int) -> Maybe (Int, (Int, Int))
    bfs [] _ = Nothing
    bfs ((b1, b2, actions):queue) visited
      | b1 == target = Just (actions, (b1, b2))
      | b2 == target = Just (actions, (b1, b2))
      | otherwise = bfs (queue ++ newStates) newVisited
      where
        newStates = filter (\(x, y, _) -> Set.notMember (x, y) visited) $ nextStates (b1, b2, actions)
        newVisited = Set.insert (b1, b2) visited

    nextStates :: State -> [State]
    nextStates (b1, b2, actions) =
      [ (cap1, b2, actions + 1)   -- Fill bucket 1
      , (b1, cap2, actions + 1)   -- Fill bucket 2
      , (0, b2, actions + 1)      -- Empty bucket 1
      , (b1, 0, actions + 1)      -- Empty bucket 2
      , (b1 - min b1 (cap2 - b2), b2 + min b1 (cap2 - b2), actions + 1) -- Pour from bucket 1 to bucket 2
      , (b1 + min b2 (cap1 - b1), b2 - min b2 (cap1 - b1), actions + 1) -- Pour from bucket 2 to bucket 1
      ]
