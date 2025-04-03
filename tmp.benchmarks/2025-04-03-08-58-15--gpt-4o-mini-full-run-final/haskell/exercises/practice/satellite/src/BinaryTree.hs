module BinaryTree (BinaryTree(..)) where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
