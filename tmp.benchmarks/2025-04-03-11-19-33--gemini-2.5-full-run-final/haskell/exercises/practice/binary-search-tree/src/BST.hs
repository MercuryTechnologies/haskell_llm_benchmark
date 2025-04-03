module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

-- A Binary Search Tree is either Empty or a Node containing
-- a value 'a', a left subtree, and a right subtree.
-- The deriving (Eq, Show) part automatically generates functions
-- to check for equality and to convert the tree to a string representation.
data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

-- Returns the left subtree.
-- If the tree is Empty, there's no node, hence no left subtree, so Nothing.
-- If it's a Node, it has a left subtree (which might be Empty), so Just l.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ l _) = Just l

-- Returns the right subtree.
-- Similar logic to bstLeft.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ r) = Just r

-- Returns the value at the root of the tree.
-- If the tree is Empty, there's no value, so Nothing.
-- If it's a Node, return Just the value.
bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node v _ _) = Just v

-- Represents an empty tree.
empty :: BST a
empty = Empty

-- Converts a list into a BST.
-- Uses foldl to iteratively insert each element from the list into the tree,
-- starting with an empty tree. `flip insert` is used because foldl passes
-- the accumulator (the tree) as the first argument to the function,
-- while `insert` expects the tree as the second argument.
fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

-- Inserts a value into the BST, maintaining the BST property.
insert :: Ord a => a -> BST a -> BST a
-- If the tree is Empty, create a new Node (a singleton tree) with the value.
insert x Empty = singleton x
-- If the tree is not Empty (it's a Node v l r):
insert x (Node v l r)
    -- If the value x is less than or equal to the node's value v,
    -- recursively insert x into the left subtree l.
    | x <= v    = Node v (insert x l) r
    -- Otherwise (x is greater than v),
    -- recursively insert x into the right subtree r.
    | otherwise = Node v l (insert x r)

-- Creates a BST with a single node containing the given value.
-- The left and right subtrees are Empty.
singleton :: a -> BST a
singleton x = Node x Empty Empty

-- Converts a BST into a sorted list using in-order traversal.
toList :: BST a -> [a]
-- If the tree is Empty, the list is empty.
toList Empty = []
-- If the tree is a Node v l r:
-- 1. Recursively convert the left subtree `l` to a list.
-- 2. Append the current node's value `v`.
-- 3. Recursively convert the right subtree `r` to a list and append it.
toList (Node v l r) = toList l ++ [v] ++ toList r
