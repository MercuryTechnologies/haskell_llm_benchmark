module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | otherwise = treeFromTraversals' preorder inorder

treeFromTraversals' :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals' [] [] = Just Empty
treeFromTraversals' (p:ps) inorder =
  case elemIndex p inorder of
    Just idx -> do
      let (leftInorder, rightInorder) = splitAt idx inorder
      let (leftPreorder, rightPreorder) = splitAt (length leftInorder) ps
      left <- treeFromTraversals' leftPreorder leftInorder
      right <- treeFromTraversals' rightPreorder (drop 1 rightInorder)
      return (Branch p left right)
    Nothing -> Nothing
treeFromTraversals' _ _ = Nothing
