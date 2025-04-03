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

-- We define a simple singly linked list data structure:
data LinkedList a
    = Nil
    | Cons a (LinkedList a)
    deriving (Eq, Show)

-- Returns the value at the head of the list
datum :: LinkedList a -> a
datum (Cons x _) = x
datum Nil        = error "No datum for an empty list."

-- Creates a linked list from a Haskell list
fromList :: [a] -> LinkedList a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

-- Checks if the linked list is empty (Nil)
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- Creates a new list node
new :: a -> LinkedList a -> LinkedList a
new x linkedList = Cons x linkedList

-- Returns the tail of the list (the list after the first element)
next :: LinkedList a -> LinkedList a
next (Cons _ xs) = xs
next Nil         = error "No next for an empty list."

-- A constant representing an empty (Nil) list
nil :: LinkedList a
nil = Nil

-- Reverses the linked list
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = go linkedList Nil
  where
    go Nil acc         = acc
    go (Cons x xs) acc = go xs (Cons x acc)

-- Converts the linked list back into a Haskell list
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs
