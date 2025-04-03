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

data LinkedList a = Node a (LinkedList a) | Empty deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Node x _) = x
datum Empty = error "The list is empty."

fromList :: [a] -> LinkedList a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next (Node _ nextNode) = nextNode
next Empty = error "The list is empty."

nil :: LinkedList a
nil = Empty

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = reverseHelper linkedList nil
  where
    reverseHelper Empty acc = acc
    reverseHelper (Node x xs) acc = reverseHelper xs (Node x acc)

toList :: LinkedList a -> [a]
toList Empty = []
toList (Node x xs) = x : toList xs
