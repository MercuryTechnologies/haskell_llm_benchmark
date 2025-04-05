module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | Node a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil = error "Empty list has no datum"
datum (Node x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr new Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Node

next :: LinkedList a -> LinkedList a
next Nil = error "Empty list has no next node"
next (Node _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
