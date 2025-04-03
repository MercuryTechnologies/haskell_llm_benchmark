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

-- A Crumb represents a single step in our path, storing the information
-- needed to reconstruct the parent node
data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- Value and right subtree
             | RightCrumb a (Maybe (BinTree a)) -- Value and left subtree
             deriving (Eq, Show)

-- A Zipper consists of the current focus (a BinTree) and the path back to the root
data Zipper a = Zipper (BinTree a) [Crumb a]
              deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree zipper = toTree (fromJust $ up zipper)
  where fromJust (Just x) = x
        fromJust Nothing = error "Unexpected Nothing in toTree"

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v Nothing _) _) = Nothing
left (Zipper (BT v (Just l) r) crumbs) = 
  Just (Zipper l (LeftCrumb v r : crumbs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v _ Nothing) _) = Nothing
right (Zipper (BT v l (Just r)) crumbs) = 
  Just (Zipper r (RightCrumb v l : crumbs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (LeftCrumb v r : crumbs)) = 
  Just (Zipper (BT v (Just tree) r) crumbs)
up (Zipper tree (RightCrumb v l : crumbs)) = 
  Just (Zipper (BT v l (Just tree)) crumbs)

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) crumbs) = Zipper (BT x l r) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) crumbs) = Zipper (BT v l r) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) crumbs) = Zipper (BT v l r) crumbs
