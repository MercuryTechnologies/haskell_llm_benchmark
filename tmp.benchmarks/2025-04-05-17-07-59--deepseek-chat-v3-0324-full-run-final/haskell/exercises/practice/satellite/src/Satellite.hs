module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Leaf
treeFromTraversals [x] [y] 
    | x == y = Just (Branch x Leaf Leaf)
    | otherwise = Nothing
treeFromTraversals (x:xs) inOrder =
    case splitAt (length leftInOrder) xs of
        (leftPre, rightPre) -> do
            leftTree <- treeFromTraversals leftPre leftInOrder
            rightTree <- treeFromTraversals rightPre rightInOrder
            return (Branch x leftTree rightTree)
    where
        (leftInOrder, _:rightInOrder) = break (==x) inOrder
        leftSize = length leftInOrder
treeFromTraversals _ _ = Nothing
