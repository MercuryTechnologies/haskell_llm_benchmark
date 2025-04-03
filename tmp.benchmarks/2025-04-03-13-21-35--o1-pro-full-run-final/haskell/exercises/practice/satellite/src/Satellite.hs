module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _  = Nothing
treeFromTraversals _  [] = Nothing
treeFromTraversals (root:pre) inorder =
    let (leftIn, pivotAndRight) = break (== root) inorder
    in case pivotAndRight of
         [] -> Nothing
         (_:rightIn) ->
             let leftCount = length leftIn
                 (leftPre, rightPre) = splitAt leftCount pre
             in do
                 leftTree  <- treeFromTraversals leftPre leftIn
                 rightTree <- treeFromTraversals rightPre rightIn
                 return (Node root leftTree rightTree)
