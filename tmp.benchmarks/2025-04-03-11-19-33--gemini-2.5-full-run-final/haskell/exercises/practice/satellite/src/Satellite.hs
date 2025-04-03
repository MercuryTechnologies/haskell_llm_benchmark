module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex, splitAt) -- Import necessary functions

-- | Reconstructs a BinaryTree from its pre-order and in-order traversals.
-- Returns Nothing if the traversals are inconsistent (e.g., different lengths,
-- or elements mismatch in a way that prevents reconstruction).
-- Requires Eq constraint to find the root element in the inorder list.
treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  -- Basic validation: Traversals must have the same length.
  | length preorder /= length inorder = Nothing
  | otherwise = buildTree preorder inorder
  where
    -- Helper function to perform the recursive reconstruction.
    -- Takes the preorder and inorder traversals of the current subtree.
    buildTree :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
    -- Base case: If both traversals are empty, we've successfully built a Leaf subtree.
    buildTree [] [] = Just Leaf -- Use Leaf constructor

    -- Recursive step: Process non-empty traversals.
    buildTree (p:ps) ino = do -- 'p' is the root of the current subtree, 'ps' is the rest of the preorder list.
        -- Find the index of the root 'p' within the inorder traversal 'ino'.
        -- If 'p' is not found in 'ino', elemIndex returns Nothing,
        -- and the 'do' block will result in Nothing, indicating invalid traversals.
        rootIndex <- elemIndex p ino

        -- Split the inorder traversal into parts left and right of the root.
        -- leftInorder: elements before the root.
        -- rightInorder': elements from the root onwards.
        let (leftInorder, rightInorder') = splitAt rootIndex ino

        -- Extract the inorder traversal for the right subtree.
        -- This involves removing the root element ('p') from rightInorder'.
        let rightInorder = case rightInorder' of
                             [] -> []     -- This case should not happen if rootIndex was valid for a non-empty ino.
                             (_:rs) -> rs -- 'rs' are the elements to the right of the root.

        -- Determine the size of the left subtree based on the inorder split.
        let leftSize = length leftInorder

        -- Split the rest of the preorder traversal ('ps') into parts for the left and right subtrees.
        -- The first 'leftSize' elements of 'ps' form the preorder traversal of the left subtree.
        -- The remaining elements form the preorder traversal of the right subtree.
        let (leftPreorder, rightPreorder) = splitAt leftSize ps

        -- Recursively call buildTree to construct the left and right subtrees.
        -- If either recursive call returns Nothing, the 'do' block propagates the Nothing.
        leftSubtree <- buildTree leftPreorder leftInorder
        rightSubtree <- buildTree rightPreorder rightInorder

        -- If both subtrees are successfully built, combine them with the root 'p'
        -- into a new Branch and return it wrapped in Just.
        return (Branch leftSubtree p rightSubtree) -- Use Branch constructor

    -- Removed redundant patterns as they are covered by the logic above
    -- and the initial length check. If elemIndex fails or lengths mismatch
    -- during recursion in an unexpected way, the Maybe monad handles it.
    buildTree _ _ = Nothing -- Catch-all for any unexpected empty list scenarios not covered above.
                            -- This primarily handles the case where the initial lists might be non-empty
                            -- but one becomes empty while the other doesn't during recursion,
                            -- which *shouldn't* happen with correct logic but acts as a safeguard.
                            -- Specifically, it catches `buildTree [] (_:_)` and `buildTree (_:_) []`.

