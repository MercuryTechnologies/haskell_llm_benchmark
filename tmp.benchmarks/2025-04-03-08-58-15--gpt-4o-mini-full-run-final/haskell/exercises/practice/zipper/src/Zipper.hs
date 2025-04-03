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
 , insertBefore
 , insertAfter
 , delete
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Zipper a = Zipper { focus :: BinTree a
                        , parent :: Maybe (Zipper a)
                        , leftSiblings :: [BinTree a]
                        , rightSiblings :: [BinTree a]
                        } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree Nothing [] []

toTree :: Zipper a -> BinTree a
toTree (Zipper f p ls rs) = case p of
    Nothing -> f
    Just parentZipper -> case leftSiblings parentZipper of
        [] -> BT (btValue f) (Just (toTree (Zipper (head (rightSiblings parentZipper)) (parent parentZipper) (tail (leftSiblings parentZipper)) []))) (Just (toTree parentZipper))
        _ -> BT (btValue f) (Just (toTree (Zipper (head ls) (parent parentZipper) (tail ls) (focus parentZipper : rightSiblings parentZipper)))) (Just (toTree parentZipper))

value :: Zipper a -> a
value zipper = btValue (focus zipper)

left :: Zipper a -> Maybe (Zipper a)
left (Zipper f p (l:ls) rs) = Just $ Zipper l p ls (focus zipper : rs)
left _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper f p ls (r:rs)) = Just $ Zipper r p (focus zipper : ls) rs
right _ = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Zipper f (Just p) ls rs) = Just $ Zipper (btValue p) (parent p) ls (focus p : rightSiblings p)
up _ = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x zipper = zipper { focus = (focus zipper) { btValue = x } }

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree zipper = zipper { focus = (focus zipper) { btLeft = tree } }

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree zipper = zipper { focus = (focus zipper) { btRight = tree } }

insertBefore :: BinTree a -> Zipper a -> Zipper a
insertBefore newTree zipper = zipper { leftSiblings = newTree : leftSiblings zipper }

insertAfter :: BinTree a -> Zipper a -> Zipper a
insertAfter newTree zipper = zipper { rightSiblings = newTree : rightSiblings zipper }

delete :: Zipper a -> Maybe (Zipper a)
delete zipper = case rightSiblings zipper of
    (r:rs) -> Just $ zipper { focus = r, rightSiblings = rs }
    [] -> case leftSiblings zipper of
        (l:ls) -> Just $ zipper { focus = l, leftSiblings = ls }
        [] -> up zipper
