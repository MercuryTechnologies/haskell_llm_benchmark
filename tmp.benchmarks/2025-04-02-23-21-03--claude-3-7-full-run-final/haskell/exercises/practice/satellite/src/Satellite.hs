module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- | Reconstructs a binary tree from its pre-order and in-order traversals.
-- The first element of the pre-order traversal is the root of the tree.
-- The position of the root in the in-order traversal divides the tree into left and right subtrees.
treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals (rootVal:preRest) inorder = do
    rootIndex <- elemIndex rootVal inorder
    
    let (leftInorder, rightInorder) = splitAt rootIndex inorder
        leftSize = length leftInorder
        (leftPreorder, rightPreorder) = splitAt leftSize preRest
    
    leftSubtree <- treeFromTraversals leftPreorder leftInorder
    rightSubtree <- treeFromTraversals rightPreorder (tail rightInorder)
    
    return $ Node rootVal leftSubtree rightSubtree
