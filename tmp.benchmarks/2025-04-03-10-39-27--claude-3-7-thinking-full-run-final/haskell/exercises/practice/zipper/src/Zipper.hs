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

data Direction = L | R deriving (Eq, Show)
data Crumb a = Crumb Direction a (Maybe (BinTree a)) deriving (Eq, Show)
data Zipper a = Zipper (BinTree a) [Crumb a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z = case up z of
             Just z' -> toTree z'
             Nothing -> error "Unexpected: up returned Nothing for a non-root zipper."

value :: Zipper a -> a
value (Zipper t _) = btValue t

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v Nothing _) _) = Nothing
left (Zipper (BT v (Just l) r) bs) = Just (Zipper l (Crumb L v r : bs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v _ Nothing) _) = Nothing
right (Zipper (BT v l (Just r)) bs) = Just (Zipper r (Crumb R v l : bs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t ((Crumb L v r):bs)) = Just (Zipper (BT v (Just t) r) bs)
up (Zipper t ((Crumb R v l):bs)) = Just (Zipper (BT v l (Just t)) bs)

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft ml (Zipper (BT v _ r) bs) = Zipper (BT v ml r) bs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight mr (Zipper (BT v l _) bs) = Zipper (BT v l mr) bs
