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

-- The Crumb type keeps track of the path back to the root.
data Crumb a = LeftCrumb a (Maybe (BinTree a))
             | RightCrumb a (Maybe (BinTree a))
             deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

-- The Zipper contains the focus and the breadcrumbs (path back to root)
data Zipper a = Zipper (BinTree a) (Breadcrumbs a)
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z = toTree' z
  where
    toTree' (Zipper t (LeftCrumb v r : bs)) =
      toTree' (Zipper (BT v (Just t) r) bs)
    toTree' (Zipper t (RightCrumb v l : bs)) =
      toTree' (Zipper (BT v l (Just t)) bs)
    toTree' (Zipper t []) = t

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just l) r) bs) = Just $ Zipper l (LeftCrumb v r : bs)
left _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l (Just r)) bs) = Just $ Zipper r (RightCrumb v l : bs)
right _ = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Zipper t (LeftCrumb v r : bs)) =
  Just $ Zipper (BT v (Just t) r) bs
up (Zipper t (RightCrumb v l : bs)) =
  Just $ Zipper (BT v l (Just t)) bs
up (Zipper _ []) = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) bs) = Zipper (BT v l r) bs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) bs) = Zipper (BT v l r) bs
