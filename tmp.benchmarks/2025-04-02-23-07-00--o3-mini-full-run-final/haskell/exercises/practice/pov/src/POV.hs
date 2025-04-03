module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node), rootLabel, subForest)

-- | Find the path from the root to the node with the given value.
-- The path is returned as a list of trees from the original root down to the found node.
findPath :: Eq a => a -> Tree a -> Maybe [Tree a]
findPath x node@(Node label forest)
  | x == label = Just [node]
  | otherwise  = foldr helper Nothing forest
  where
    helper child acc =
      case acc of
        Just _  -> acc
        Nothing -> case findPath x child of
                     Just path -> Just (node : path)
                     Nothing   -> Nothing

-- | Reassemble the tree by reversing the parent-child relationships along the given path.
-- The path should be a list of nodes from the original root down to the chosen node.
reassemble :: Eq a => [Tree a] -> Tree a
reassemble [] = error "reassemble: empty path"
reassemble path =
  let rev = reverse path
  in foldl (\acc node ->
              let Node pLabel pForest = node
                  filtered = filter (\child -> rootLabel child /= rootLabel acc) pForest
              in Node (rootLabel acc) (subForest acc ++ [Node pLabel filtered])
           ) (head rev) (tail rev)

-- | Re-orient the input tree so that the node with the given value becomes the new root.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- findPath x tree
  return $ reassemble path

-- | Search for a node with the given value in a tree and return the path to it.
searchPath :: Eq a => a -> Tree a -> Maybe [a]
searchPath x (Node label forest)
  | x == label = Just [label]
  | otherwise  = foldr helper Nothing forest
  where
    helper child acc =
      case acc of
        Just _  -> acc
        Nothing -> case searchPath x child of
                     Just path -> Just (label : path)
                     Nothing   -> Nothing

-- | Find the path between two nodes in the tree.
-- The function works by re-rooting the tree at 'from' and then searching for 'to' in the reoriented tree.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  newTree <- fromPOV from tree
  searchPath to newTree
