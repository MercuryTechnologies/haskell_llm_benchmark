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

data Zipper a = Zipper (BinTree a) (Breadcrumbs a)
              deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z = toTree (goUp z)
  where
    goUp (Zipper t (LeftCrumb v r : bs)) =
      Zipper (BT v (Just t) r) bs
    goUp (Zipper t (RightCrumb v l : bs)) =
      Zipper (BT v l (Just t)) bs
    goUp z' = z'  -- root reached

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT _ Nothing _) _) = Nothing
left (Zipper (BT v (Just lft) rgt) crumbs) =
  Just (Zipper lft (LeftCrumb v rgt : crumbs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT _ _ Nothing) _) = Nothing
right (Zipper (BT v lft (Just rgt)) crumbs) =
  Just (Zipper rgt (RightCrumb v lft : crumbs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (LeftCrumb v r : bs)) =
  Just (Zipper (BT v (Just t) r) bs)
up (Zipper t (RightCrumb v l : bs)) =
  Just (Zipper (BT v l (Just t)) bs)

setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper (BT _ l r) crumbs) =
  Zipper (BT newVal l r) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) crumbs) =
  Zipper (BT v newLeft r) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) crumbs) =
  Zipper (BT v l newRight) crumbs
