module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _  = Nothing
treeFromTraversals _  [] = Nothing
treeFromTraversals (p:ps) inorder =
    case break (== p) inorder of
        (_, []) -> Nothing  -- root not found in inorder
        (leftInorder, _:rightInorder) ->
            let leftPreorderLen = length leftInorder
                (leftPreorder, rightPreorder) = splitAt leftPreorderLen ps
            in do
                leftSubtree <- treeFromTraversals leftPreorder leftInorder
                rightSubtree <- treeFromTraversals rightPreorder rightInorder
                return (Node p leftSubtree rightSubtree)
