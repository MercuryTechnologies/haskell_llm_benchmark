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

data BinTree a = BT
  { btValue :: a
  , btLeft  :: Maybe (BinTree a)
  , btRight :: Maybe (BinTree a)
  } deriving (Eq, Show)

data Crumb a
  = LeftCrumb a (Maybe (BinTree a))
  | RightCrumb a (Maybe (BinTree a))
  deriving (Eq, Show)

data Zipper a
  = Z (BinTree a) [Crumb a]
  | Dummy
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Z tree []

toTree :: Zipper a -> BinTree a
toTree Dummy = error "Cannot convert dummy zipper to a tree."
toTree z@(Z t []) = t
toTree z =
  case up z of
    Nothing -> case z of
                 Z t' _ -> t'
                 Dummy -> error "Unexpected dummy zipper."
    Just z' -> toTree z'

value :: Zipper a -> a
value (Z (BT v _ _) _) = v
value Dummy = error "Cannot get value from dummy zipper."

left :: Zipper a -> Maybe (Zipper a)
left (Z (BT v l r) bs) =
  case l of
    Nothing -> Nothing
    Just ltree -> Just (Z ltree (LeftCrumb v r : bs))
left Dummy = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Z (BT v l r) bs) =
  case r of
    Nothing -> Nothing
    Just rtree -> Just (Z rtree (RightCrumb v l : bs))
right Dummy = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Z _ []) = Nothing
up (Z t (LeftCrumb v r : bs))  =
  Just (Z (BT v (Just t) r) bs)
up (Z t (RightCrumb v l : bs)) =
  Just (Z (BT v l (Just t)) bs)
up Dummy = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Z (BT _ l r) bs) = Z (BT x l r) bs
setValue _ Dummy = Dummy

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Z (BT v _ r) bs) = Z (BT v newLeft r) bs
setLeft _ Dummy = Dummy

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Z (BT v l _) bs) = Z (BT v l newRight) bs
setRight _ Dummy = Dummy
