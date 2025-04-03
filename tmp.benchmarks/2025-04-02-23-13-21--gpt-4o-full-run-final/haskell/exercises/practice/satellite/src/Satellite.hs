module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals (p:ps) inorder =
    case span (/= p) inorder of
        (left, _:right) -> do
            leftTree <- treeFromTraversals (take (length left) ps) left
            rightTree <- treeFromTraversals (drop (length left) ps) right
            return $ Node p leftTree rightTree
        _ -> Nothing
