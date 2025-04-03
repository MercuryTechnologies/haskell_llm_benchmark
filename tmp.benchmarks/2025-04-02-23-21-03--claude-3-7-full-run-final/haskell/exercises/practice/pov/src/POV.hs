module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe)

-- | Reparent the tree so that the specified node is the new root
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree
  | rootLabel tree == x = Just tree  -- If x is already the root, return the tree
  | otherwise = findAndReparent x tree

-- | Find a node in the tree and reparent the tree with that node as the root
findAndReparent :: Eq a => a -> Tree a -> Maybe (Tree a)
findAndReparent x tree = listToMaybe $ findAndReparentHelper x tree []

-- | Helper function that keeps track of the path to the current node
findAndReparentHelper :: Eq a => a -> Tree a -> [Tree a] -> [Tree a]
findAndReparentHelper x tree path
  | rootLabel tree == x = [reparent tree path]  -- Found the node, reparent
  | otherwise = concatMap (\child -> findAndReparentHelper x child (tree:path)) (subForest tree)

-- | Reparent the tree with the given node as the root
reparent :: Eq a => Tree a -> [Tree a] -> Tree a
reparent node [] = node  -- No path, return the node
reparent node (parent:ancestors) =
  let
    -- Create a new parent tree without the current node in its children
    newParent = parent { subForest = filter (\t -> rootLabel t /= rootLabel node) (subForest parent) }
    -- Add the reparented ancestors to the node's children
    newNode = node { subForest = reparent newParent ancestors : subForest node }
  in newNode

-- | Trace the path between two nodes in the tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  -- Reparent the tree with 'from' as the root
  fromTree <- fromPOV from tree
  -- Find the path from the new root to 'to'
  path <- findPath to [] fromTree
  -- Return the path
  return path

-- | Find the path from the root of the tree to the target node
findPath :: Eq a => a -> [a] -> Tree a -> Maybe [a]
findPath target path tree
  | rootLabel tree == target = Just (reverse (target:path))
  | otherwise = listToMaybe $ mapMaybe (\child -> findPath target (rootLabel tree:path) child) (subForest tree)

-- | Apply a function to each element of a list and collect the Just results
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (:acc) (f x)) []
