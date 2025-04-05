module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe, mapMaybe)

-- Re-root the tree at the node with value x
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = reroot [] tree
  where
    -- reroot accumulates the path from the original root to the target node
    reroot :: Eq a => [Tree a] -> Tree a -> Maybe (Tree a)
    reroot path (Node label children)
      | label == x = Just (buildNewRoot (Node label children) path)
      | otherwise = listToMaybe $ mapMaybe (\child -> reroot (Node label (filter (/= child) children) : path) child) children

    -- buildNewRoot takes the found node and the path back to the original root, and reconstructs the tree
    buildNewRoot :: Tree a -> [Tree a] -> Tree a
    buildNewRoot current [] = current
    buildNewRoot current (Node label siblings : rest) =
      buildNewRoot (Node label (current : siblings)) rest

-- Find the path between two nodes
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  newRoot <- fromPOV from tree
  findPath to newRoot

-- Find path from root to target node
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node label children)
  | label == target = Just [label]
  | otherwise = listToMaybe $ mapMaybe tryChild children
  where
    tryChild child = do
      path <- findPath target child
      return (label : path)
