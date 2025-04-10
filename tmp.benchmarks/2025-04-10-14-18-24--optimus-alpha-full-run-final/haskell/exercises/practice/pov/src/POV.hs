module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (mapMaybe, listToMaybe)

-- Find the path from the root to the node with value x
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath x (Node v children)
  | x == v    = Just [v]
  | otherwise = case mapMaybe (findPath x) children of
                  []     -> Nothing
                  (p:_)  -> Just (v : p)

-- Find the path from the root to the node with value x, returning the nodes themselves
findNodePath :: Eq a => a -> Tree a -> Maybe [Tree a]
findNodePath x n@(Node v children)
  | x == v    = Just [n]
  | otherwise = case mapMaybe (findNodePath x) children of
                  []    -> Nothing
                  (p:_) -> Just (n : p)

-- Remove a child node from a tree, returning the child and the tree with that child removed
extractChild :: Eq a => a -> Tree a -> Maybe (Tree a, Tree a)
extractChild x (Node v children) =
  case break ((== x) . rootLabel) children of
    (_, []) -> Nothing
    (before, c:after) -> Just (c, Node v (before ++ after))

-- Rebuild the tree so that the node with value x is the new root
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- findNodePath x tree
  reparent path
  where
    reparent [n] = Just n
    reparent (parent@(Node pv children) : child@(Node cv gc) : rest) = do
      -- Remove parent from child's children if present
      let newChild = Node cv (gc ++ [Node pv (filter (/= child) children)])
      reparent (newChild : rest)
    reparent _ = Nothing

-- Find the path between two nodes as a list of values
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  newTree <- fromPOV from tree
  findPath to newTree
