module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))

-- | Find the path (list of Tree nodes) from the root to the node whose label matches 'x'.
findPath :: Eq a => a -> Tree a -> Maybe [Tree a]
findPath x t@(Node lbl children)
  | x == lbl  = Just [t]
  | otherwise = foldr trySubtree Nothing children
  where
    trySubtree subtree acc =
      case findPath x subtree of
        Nothing     -> acc
        Just result -> Just (t : result)

-- | Given a path from the root down to a desired node, rebuild the tree so that
--   the desired node is the new root.
reRoot :: Eq a => [Tree a] -> Tree a
reRoot []       = error "reRoot called with empty path, which should be impossible."
reRoot [t]      = t
reRoot (t1:t2:ts) =
    let label    = rootLabel t1
        children = filter ((/= rootLabel t2) . rootLabel) (subForest t1)
        newNode  = Node label children
    in reRoot (Node (rootLabel t2) (newNode : filter ((/= label) . rootLabel) (subForest t2)) : ts)

-- | Re-orient the tree so that 'x' is the new root.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- findPath x tree
  pure (reRoot path)

-- | Find the path from 'from' to 'to' after re-orienting the tree to 'from'.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  reoriented <- fromPOV from tree
  path       <- findPath to reoriented
  pure (map rootLabel path)
