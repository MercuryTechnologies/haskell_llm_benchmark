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

-- Define the LinkedList data type
-- It can be either Nil (empty) or a Node containing a value 'a'
-- and the rest of the list (LinkedList a).
data LinkedList a = Nil | Node a (LinkedList a) deriving (Eq, Show)

-- Returns the value/datum of the head node.
-- Errors if the list is empty (Nil).
datum :: LinkedList a -> a
datum (Node x _) = x
datum Nil        = error "datum: list is empty"

-- Converts a standard Haskell list to our LinkedList.
-- Uses foldr to efficiently build the list from right to left.
fromList :: [a] -> LinkedList a
fromList = foldr new nil
-- Alternative recursive implementation:
-- fromList [] = Nil
-- fromList (x:xs) = Node x (fromList xs)

-- Checks if the LinkedList is empty (Nil).
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- Creates a new LinkedList node, prepending the value 'x'
-- to the existing 'linkedList'.
new :: a -> LinkedList a -> LinkedList a
new = Node -- This is equivalent to: new x linkedList = Node x linkedList

-- Returns the rest of the list (the next node).
-- Errors if the list is empty (Nil).
next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil         = error "next: list is empty"

-- Represents the empty LinkedList.
nil :: LinkedList a
nil = Nil

-- Reverses the order of nodes in the LinkedList.
-- Uses a tail-recursive helper function 'go' with an accumulator.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = go list nil
  where
    go :: LinkedList a -> LinkedList a -> LinkedList a
    go Nil         acc = acc
    go (Node x xs) acc = go xs (Node x acc) -- Prepend current element to the accumulator

-- Converts our LinkedList back to a standard Haskell list.
-- Uses recursion to traverse the list.
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Node x xs) = x : toList xs
