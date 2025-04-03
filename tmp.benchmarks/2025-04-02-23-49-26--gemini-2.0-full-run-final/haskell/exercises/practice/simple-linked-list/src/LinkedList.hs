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
datum (Node x _) = x
datum Nil = error "No datum for Nil"

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next (Node _ nextNode) = nextNode
next Nil = error "No next for Nil"

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = reverseHelper linkedList Nil

reverseHelper :: LinkedList a -> LinkedList a -> LinkedList a
reverseHelper Nil acc = acc
reverseHelper (Node x nextNode) acc = reverseHelper nextNode (Node x acc)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x nextNode) = x : toList nextNode
