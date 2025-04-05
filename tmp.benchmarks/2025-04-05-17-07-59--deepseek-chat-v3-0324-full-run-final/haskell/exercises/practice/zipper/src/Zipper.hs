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

data Crumb a = LeftCrumb a (Maybe (BinTree a)) 
             | RightCrumb a (Maybe (BinTree a))
             deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

data Zipper a = Zipper { focus :: BinTree a
                       , crumbs :: Breadcrumbs a
                       } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree zipper = toTree (maybe zipper id (up zipper))

value :: Zipper a -> a
value = btValue . focus

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT _ Nothing _) _) = Nothing
left (Zipper (BT x (Just l) r) bs) = Just $ Zipper l (LeftCrumb x r:bs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT _ _ Nothing) _) = Nothing
right (Zipper (BT x l (Just r)) bs) = Just $ Zipper r (RightCrumb x l:bs)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (LeftCrumb x r:bs)) = Just $ Zipper (BT x (Just t) r) bs
up (Zipper t (RightCrumb x l:bs)) = Just $ Zipper (BT x l (Just t)) bs

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT x _ r) bs) = Zipper (BT x l r) bs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT x l _) bs) = Zipper (BT x l r) bs
