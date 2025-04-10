module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree, empty, node)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just empty
treeFromTraversals [] _  = Nothing
treeFromTraversals _  [] = Nothing
treeFromTraversals (root:preorder) inorder =
    case break (== root) inorder of
        (_, []) -> Nothing  -- root not found in inorder
        (leftInorder, _:rightInorder) ->
            let leftSize = length leftInorder
                (leftPreorder, rightPreorder) = splitAt leftSize preorder
            in do
                leftTree <- treeFromTraversals leftPreorder leftInorder
                rightTree <- treeFromTraversals rightPreorder rightInorder
                return (node root leftTree rightTree)
