module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _  = Nothing
treeFromTraversals _  [] = Nothing
treeFromTraversals (p:ps) inorder =
  case break (== p) inorder of
    (_, []) -> Nothing
    (leftInorder, _:rightInorder) ->
      let leftCount = length leftInorder
          (leftPre, rightPre) = splitAt leftCount ps
      in do
           leftTree <- treeFromTraversals leftPre leftInorder
           rightTree <- treeFromTraversals rightPre rightInorder
           return (Node p leftTree rightTree)
