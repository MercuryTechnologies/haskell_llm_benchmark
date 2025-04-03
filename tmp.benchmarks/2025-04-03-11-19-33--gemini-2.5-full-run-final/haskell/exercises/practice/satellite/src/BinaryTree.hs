module BinaryTree (BinaryTree(..)) where

-- | A simple binary tree data structure.
-- 'Leaf' represents a leaf node or an empty tree.
-- 'Branch' represents an internal node with a value and two children.
-- Renamed constructors to match test expectations.
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)
