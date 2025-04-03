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

data Crumb a
  = LeftCrumb a (Maybe (BinTree a))
  | RightCrumb a (Maybe (BinTree a))
  deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Crumb a]
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z =
  case up z of
    Nothing -> let (Zipper t' _) = z in t'
    Just z' -> toTree z'

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) bs) =
  case l of
    Nothing     -> Nothing
    Just lTree  -> Just (Zipper lTree (LeftCrumb v r : bs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) bs) =
  case r of
    Nothing      -> Nothing
    Just rTree   -> Just (Zipper rTree (RightCrumb v l : bs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (LeftCrumb v r : bs)) =
  Just (Zipper (BT v (Just t) r) bs)
up (Zipper t (RightCrumb v l : bs)) =
  Just (Zipper (BT v l (Just t)) bs)

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) =
  Zipper (BT x l r) bs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft subtree (Zipper (BT v _ r) bs) =
  Zipper (BT v subtree r) bs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight subtree (Zipper (BT v l _) bs) =
  Zipper (BT v l subtree) bs
