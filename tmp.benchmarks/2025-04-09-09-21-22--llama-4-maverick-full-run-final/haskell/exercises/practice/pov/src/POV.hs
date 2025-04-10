module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = reparent <$> findSubtree x tree
  where
    reparent (Node _ subtrees) = Node x (reparentSubtree x <$> subtrees)
    reparentSubtree x (Node y subtrees)
      | x == y = Node x (reparentSubtree x <$> subtrees)
      | otherwise = Node y (reparentSubtree x <$> subtrees)
    findSubtree x (Node y subtrees)
      | x == y = Just (Node y subtrees)
      | otherwise = findSubtreeInSubtrees x subtrees
    findSubtreeInSubtrees x [] = Nothing
    findSubtreeInSubtrees x (t:ts) =
      case findSubtree x t of
        Just subtree -> Just subtree
        Nothing -> findSubtreeInSubtrees x ts

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = error "You need to implement this function."
