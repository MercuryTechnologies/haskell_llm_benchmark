module Satellite (BinaryTree(..), treeFromTraversals) where

data BinaryTree a
  = Empty
  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _  = Nothing
treeFromTraversals _ []  = Nothing
treeFromTraversals (root:pre) inorder =
    case break (== root) inorder of
        (_, []) -> Nothing
        (leftInorder, _ : rightInorder) ->
            let leftCount = length leftInorder
                (leftPre, rightPre) = splitAt leftCount pre
            in if length leftPre /= leftCount
                then Nothing
                else do
                    leftTree  <- treeFromTraversals leftPre leftInorder
                    rightTree <- treeFromTraversals rightPre rightInorder
                    return $ Node root leftTree rightTree
