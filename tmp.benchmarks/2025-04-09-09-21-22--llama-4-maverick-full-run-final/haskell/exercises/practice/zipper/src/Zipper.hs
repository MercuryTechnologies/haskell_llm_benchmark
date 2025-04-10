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

data Dir a = LeftDir a (Maybe (BinTree a)) | RightDir a (Maybe (BinTree a)) deriving (Eq, Show)
data Zipper a = Zipper (BinTree a) [Dir a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree (Zipper tree (dir:dirs)) = case dir of
  LeftDir val right -> toTree $ Zipper (BT val (Just tree) right) dirs
  RightDir val left -> toTree $ Zipper (BT val left (Just tree)) dirs

value :: Zipper a -> a
value (Zipper tree _) = btValue tree

left :: Zipper a -> Maybe (Zipper a)
left (Zipper tree@(BT _ left _) crumbs) = case left of
  Just subtree -> Just $ Zipper subtree (LeftDir (btValue tree) (btRight tree) : crumbs)
  Nothing -> Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper tree@(BT _ _ right) crumbs) = case right of
  Just subtree -> Just $ Zipper subtree (RightDir (btValue tree) (btLeft tree) : crumbs)
  Nothing -> Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (dir:dirs)) = case dir of
  LeftDir val right -> Just $ Zipper (BT val (Just tree) right) dirs
  RightDir val left -> Just $ Zipper (BT val left (Just tree)) dirs

setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper tree crumbs) = Zipper (tree { btValue = newVal }) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper tree crumbs) = Zipper (tree { btLeft = newLeft }) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper tree crumbs) = Zipper (tree { btRight = newRight }) crumbs
