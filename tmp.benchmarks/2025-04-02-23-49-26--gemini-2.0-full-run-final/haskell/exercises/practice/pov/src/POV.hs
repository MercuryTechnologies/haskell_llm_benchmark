module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), rootLabel, subForest)
import Data.Maybe (listToMaybe, mapMaybe)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV newRoot tree =
  let
    findNode :: Eq a => a -> Tree a -> Maybe (Tree a, Maybe a)
    findNode target (Node label children)
      | label == target = Just (Node label [], Nothing)
      | otherwise =
        case findChild target children of
          Just (child, parent) -> Just (child, Just label)
          Nothing -> Nothing

    findChild :: Eq a => a -> [Tree a] -> Maybe (Tree a, Maybe a)
    findChild _ [] = Nothing
    findChild target (t:ts) =
      case findNode target t of
        Just (child, parent) -> Just (child, parent)
        Nothing -> findChild target ts

    reconstructTree :: Eq a => Tree a -> Maybe a -> Tree a
    reconstructTree (Node label children) parent =
      Node label $ case parent of
        Nothing -> children
        Just p -> [Node p [Node label children]]

    extractAndRebuild :: Eq a => a -> Tree a -> Maybe (Tree a)
    extractAndRebuild target tree =
      case findNode target tree of
        Just (subtree, parent) ->
          let newTree = reconstructTree subtree parent
          in Just newTree
        Nothing -> Nothing
  in
    extractAndRebuild newRoot tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree =
  case fromPOV from tree of
    Just reorientedTree -> findPathTo to reorientedTree
    Nothing -> Nothing

findPathTo :: Eq a => a -> Tree a -> Maybe [a]
findPathTo target tree =
  let
    findPath :: Eq a => a -> Tree a -> [a] -> Maybe [a]
    findPath t (Node label children) path
      | label == t = Just (reverse (label : path))
      | otherwise =
        listToMaybe $ mapMaybe (\(child, parentLabel) -> findPath t child (parentLabel : path)) children'
      where
        children' = map (\x -> (x, label : path)) children
  in findPath target tree []
