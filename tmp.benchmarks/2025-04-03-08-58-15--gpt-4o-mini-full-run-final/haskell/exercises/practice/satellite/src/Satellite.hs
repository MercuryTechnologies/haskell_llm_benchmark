module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals preorder inorder = Just (buildTree preorder inorder)

buildTree :: Ord a => [a] -> [a] -> BinaryTree a
buildTree [] _ = Empty
buildTree _ [] = Empty
buildTree (p:preorder) inorder =
    let (leftInorder, _:rightInorder) = splitAt (index p inorder) inorder
        leftPreorder = take (length leftInorder) preorder
        rightPreorder = drop (length leftInorder) preorder
    in Node p (buildTree leftPreorder leftInorder) (buildTree rightPreorder rightInorder)

index :: Eq a => a -> [a] -> Int
index x xs = case elemIndex x xs of
    Just i  -> i
    Nothing -> error "Element not found"
