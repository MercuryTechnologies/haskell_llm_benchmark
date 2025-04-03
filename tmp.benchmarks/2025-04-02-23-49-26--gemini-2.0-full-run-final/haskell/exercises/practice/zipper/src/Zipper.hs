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

data Zipper a = Zipper { zFocus :: BinTree a
                       , zLeft  :: Maybe (BinTree a)
                       , zRight :: Maybe (BinTree a)
                       , zUp    :: Maybe (Zipper a)
                       } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree Nothing Nothing Nothing

toTree :: Zipper a -> BinTree a
toTree zipper = case zUp zipper of
                  Nothing -> zFocus zipper
                  Just _  -> toTree (fromFocus zipper)

value :: Zipper a -> a
value zipper = btValue (zFocus zipper)

left :: Zipper a -> Maybe (Zipper a)
left zipper = case btLeft (zFocus zipper) of
                Nothing -> Nothing
                Just l  -> Just (Zipper l Nothing Nothing (Just zipper))

right :: Zipper a -> Maybe (Zipper a)
right zipper = case btRight (zFocus zipper) of
                 Nothing -> Nothing
                 Just r  -> Just (Zipper r Nothing Nothing (Just zipper))

up :: Zipper a -> Maybe (Zipper a)
up zipper = zUp zipper

setValue :: a -> Zipper a -> Zipper a
setValue x zipper = zipper { zFocus = (zFocus zipper) { btValue = x } }

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree zipper = zipper { zFocus = (zFocus zipper) { btLeft = tree } }

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree zipper = zipper { zFocus = (zFocus zipper) { btRight = tree } }

fromFocus :: Eq a => Zipper a -> Zipper a
fromFocus zipper = case zUp zipper of
                     Nothing -> zipper
                     Just par ->
                       if Just (zFocus zipper) == btLeft (zFocus par)
                         then par { zFocus = (zFocus par) { btLeft = Just (zFocus zipper) } }
                         else par { zFocus = (zFocus par) { btRight = Just (zFocus zipper) } }
