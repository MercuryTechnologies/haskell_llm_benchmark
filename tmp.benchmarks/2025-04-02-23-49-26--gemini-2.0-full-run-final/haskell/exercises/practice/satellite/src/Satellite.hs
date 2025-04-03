module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..), Empty, Node)
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals (preHead:preTail) inorder =
  case elemIndex preHead inorder of
    Nothing -> Nothing  -- preHead not in inorder, invalid traversals
    Just index ->
      let
        leftInorder = take index inorder
        rightInorder = drop (index + 1) inorder
        leftPreorder = take (length leftInorder) preTail
        rightPreorder = drop (length leftInorder) preTail
      in
        do
          leftTree <- treeFromTraversals leftPreorder leftInorder
          rightTree <- treeFromTraversals rightPreorder rightInorder
          return (Node preHead leftTree rightTree)
treeFromTraversals _ _ = Nothing
