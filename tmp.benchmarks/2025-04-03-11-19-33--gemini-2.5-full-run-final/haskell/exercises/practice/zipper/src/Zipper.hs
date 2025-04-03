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

-- The basic data structure
data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- A breadcrumb stores the parent's value, the direction we came from,
-- and the sibling subtree we didn't take.
data Crumb a = LCrumb a (Maybe (BinTree a)) -- Came from left, stores parent value and right sibling
             | RCrumb a (Maybe (BinTree a)) -- Came from right, stores parent value and left sibling
             deriving (Eq, Show)

-- The context is a list of breadcrumbs, representing the path back to the root.
type Context a = [Crumb a]

-- The zipper itself: the focused subtree and the context.
data Zipper a = Zipper (BinTree a) (Context a) deriving (Eq, Show)

-- Create a zipper from a tree, focused on the root.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- Reconstruct the full tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper focus context) = case up (Zipper focus context) of
    Nothing -> focus -- Already at the root
    Just parentZipper -> toTree parentZipper -- Go up recursively

-- Get the value of the focused node.
value :: Zipper a -> a
value (Zipper focus _) = btValue focus

-- Move focus to the left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper focus@(BT v _ r) context) = case btLeft focus of
    Just l -> Just $ Zipper l (LCrumb v r : context) -- Add breadcrumb for going left
    Nothing -> Nothing

-- Move focus to the right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper focus@(BT v l _) context) = case btRight focus of
    Just r -> Just $ Zipper r (RCrumb v l : context) -- Add breadcrumb for going right
    Nothing -> Nothing

-- Move focus to the parent node.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing -- Already at the root, cannot go up
up (Zipper focus (LCrumb pVal pRight : cs)) = -- Was left child, reconstruct parent
    Just $ Zipper (BT pVal (Just focus) pRight) cs
up (Zipper focus (RCrumb pVal pLeft : cs)) = -- Was right child, reconstruct parent
    Just $ Zipper (BT pVal pLeft (Just focus)) cs

-- Set the value of the focused node.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) context) = Zipper (BT x l r) context

-- Set the left child of the focused node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree (Zipper (BT v _ r) context) = Zipper (BT v tree r) context

-- Set the right child of the focused node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree (Zipper (BT v l _) context) = Zipper (BT v l tree) context
