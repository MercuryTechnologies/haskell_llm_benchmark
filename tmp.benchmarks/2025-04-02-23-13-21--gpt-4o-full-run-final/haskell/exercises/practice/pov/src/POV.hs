module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (listToMaybe, mapMaybe)

-- Reparent the tree with the given node as the new root
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = reparent x tree Nothing

-- Helper function to reparent the tree
reparent :: Eq a => a -> Tree a -> Maybe (Tree a) -> Maybe (Tree a)
reparent x (Node value children) parent
  | x == value = Just $ Node value (maybeToList parent ++ children)
  | otherwise = listToMaybe $ mapMaybe (\child -> reparent x child (Just (Node value (filter (/= child) children ++ maybeToList parent)))) children

-- Trace the path between two nodes in the tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  fromTree <- fromPOV from tree
  pathToNode to fromTree

-- Helper function to find the path to a node
pathToNode :: Eq a => a -> Tree a -> Maybe [a]
pathToNode x (Node value children)
  | x == value = Just [value]
  | otherwise = fmap (value :) . listToMaybe $ mapMaybe (pathToNode x) children

-- Convert Maybe to List
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
