module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.List (elemIndex)

-- Find a node in the tree and return its path from root
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath x (Node val children)
  | x == val = Just [val]
  | otherwise = case concatMap (findPath x) children of
                  [] -> Nothing
                  (p:_) -> Just (val : p)

-- Rebuild tree from a path
rebuildTree :: Eq a => [a] -> Tree a -> Tree a
rebuildTree [] tree = tree
rebuildTree (p:ps) (Node val children) =
  case elemIndex p (map rootLabel children) of
    Nothing -> Node val children
    Just i -> 
      let child = children !! i
          siblings = take i children ++ drop (i+1) children
          rotatedChild = rebuildTree ps child
      in case rotatedChild of
           Node rChild rChildren -> 
             Node p (rChildren ++ [Node val siblings])

-- Reorient tree from given node's perspective
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = case findPath x tree of
  Nothing -> Nothing
  Just path -> Just $ rebuildTree (reverse path) tree

-- Find path between two nodes in original tree
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  pathFrom <- findPath from tree
  pathTo <- findPath to tree
  let common = last $ takeWhile (`elem` pathTo) pathFrom
  let fromPath = takeWhile (/= common) pathFrom
  let toPath = takeWhile (/= common) pathTo
  return $ reverse fromPath ++ [common] ++ toPath
