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

-- Define the singly linked list data type
data LinkedList a = Nil | Node a (LinkedList a)
    deriving (Eq, Show)

-- Get the datum (value) from the head node
datum :: LinkedList a -> a
datum (Node x _) = x
datum Nil = error "Empty list has no datum"

-- Create a linked list from a regular list
fromList :: [a] -> LinkedList a
fromList = foldr Node Nil

-- Check if the linked list is empty
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- Create a new node with value x and next node as linkedList
new :: a -> LinkedList a -> LinkedList a
new = Node

-- Get the next node in the list
next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil = error "Empty list has no next node"

-- The empty linked list
nil :: LinkedList a
nil = Nil

-- Reverse the linked list
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go acc Nil = acc
    go acc (Node x xs) = go (Node x acc) xs

-- Convert a linked list back to a regular list
toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
