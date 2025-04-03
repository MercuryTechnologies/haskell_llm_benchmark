module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- Internal type to store context (breadcrumbs) when navigating the tree.
data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- when coming from left: store parent's value and parent's right subtree
             | RightCrumb a (Maybe (BinTree a)) -- when coming from right: store parent's value and parent's left subtree
             deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Crumb a] deriving (Eq, Show)

-- Create a zipper focused on the root of the tree.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- Reconstruct the tree from the zipper by applying all breadcrumbs.
toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree (Zipper tree (crumb:crumbs)) = toTree (Zipper (reconstruct crumb tree) crumbs)

-- Helper function to reconstruct parent's node from a crumb and current focus.
reconstruct :: Crumb a -> BinTree a -> BinTree a
reconstruct (LeftCrumb parent r) t = BT parent (Just t) r
reconstruct (RightCrumb parent l) t = BT parent l (Just t)

-- Get the value at the focus.
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- Navigate to the left child, if it exists.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) crumbs) = case l of
  Just leftTree -> Just (Zipper leftTree (LeftCrumb v r : crumbs))
  Nothing       -> Nothing

-- Navigate to the right child, if it exists.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) crumbs) = case r of
  Just rightTree -> Just (Zipper rightTree (RightCrumb v l : crumbs))
  Nothing        -> Nothing

-- Move up to the parent node, if possible.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper t []) = Nothing
up (Zipper t (crumb:crumbs)) = Just (Zipper (reconstruct crumb t) crumbs)

-- Set the value at the focus.
setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper (BT _ l r) crumbs) = Zipper (BT newVal l r) crumbs

-- Set the left subtree of the focus.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) crumbs) = Zipper (BT v newLeft r) crumbs

-- Set the right subtree of the focus.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) crumbs) = Zipper (BT v l newRight) crumbs
