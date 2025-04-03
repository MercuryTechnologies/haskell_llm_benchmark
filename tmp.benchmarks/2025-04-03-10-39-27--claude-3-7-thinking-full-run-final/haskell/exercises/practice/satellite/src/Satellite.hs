module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
    | length preorder /= length inorder = Nothing
    | otherwise = buildTree preorder inorder
  where
    buildTree [] [] = Just Empty
    buildTree (root:prerest) inord = 
        case elemIndex root inord of
            Nothing -> Nothing
            Just rootIdx -> do
                let (leftInord, _:rightInord) = splitAt rootIdx inord
                    leftSize = length leftInord
                    (leftPreord, rightPreord) = splitAt leftSize prerest
                leftTree <- buildTree leftPreord leftInord
                rightTree <- buildTree rightPreord rightInord
                return $ Node root leftTree rightTree
    
    elemIndex :: Eq a => a -> [a] -> Maybe Int
    elemIndex _ [] = Nothing
    elemIndex x (y:ys)
        | x == y    = Just 0
        | otherwise = fmap (+1) (elemIndex x ys)
