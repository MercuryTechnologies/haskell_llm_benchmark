module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe)

-- Find a node in a tree and return itself along with the path to it
findNodeWithPath :: Eq a => a -> Tree a -> Maybe (Tree a, [Tree a])
findNodeWithPath x node@(Node val children)
  | x == val = Just (node, [])
  | otherwise = listToMaybe [
      (found, node:path) | child <- children,
      Just (found, path) <- [findNodeWithPath x child]
    ]

-- Reparent a tree to make a specific node the new root
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = case findNodeWithPath x tree of
  Nothing -> Nothing
  Just (Node val children, []) -> Just (Node val children)  -- The node is already the root
  Just (Node val children, path) -> Just (reparent val children path)
  where
    reparent val children [] = Node val children
    reparent val children ((Node pval pchildren):ancestors) =
      let otherChildren = filter (\(Node v _) -> v /= val) pchildren
          newParent = reparent pval otherChildren ancestors
      in Node val (newParent:children)

-- Trace the path between two nodes
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reparentedTree <- fromPOV from tree
  findPathTo to reparentedTree
  where
    findPathTo x (Node val children)
      | x == val = Just [val]
      | otherwise = listToMaybe [
          val:path | child <- children,
          Just path <- [findPathTo x child]
        ]
