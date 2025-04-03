module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..), flatten)
import Data.Maybe (listToMaybe)
import Data.List (nub)

-- Reparent the tree from the perspective of the given node
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
    path <- tracePathBetween x (rootLabel tree) tree
    let newRoot = last path
    let newTree = buildTree newRoot path tree
    return newTree

-- Build a new tree from the perspective of the new root
buildTree :: Eq a => a -> [a] -> Tree a -> Tree a
buildTree newRoot path tree = Node newRoot (map (buildSubtree newRoot tree) (tail path))

-- Build a subtree for each node in the path
buildSubtree :: Eq a => a -> Tree a -> a -> Tree a
buildSubtree newRoot tree target = case tree of
    Node label subtrees
        | label == target -> Node label (map (buildSubtree newRoot tree) (flatten (Node label subtrees)))
        | otherwise -> Node label []

-- Find the path between two nodes
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = findPath from to tree []

-- Helper function to find the path recursively
findPath :: Eq a => a -> a -> Tree a -> [a] -> Maybe [a]
findPath from to (Node label subtrees) visited
    | label == from = Just (label : visited)
    | otherwise = case listToMaybe $ concatMap (\subtree -> findPath from to subtree (label : visited)) subtrees of
        Just path -> Just path
        Nothing -> if label == to then Just (label : visited) else Nothing
