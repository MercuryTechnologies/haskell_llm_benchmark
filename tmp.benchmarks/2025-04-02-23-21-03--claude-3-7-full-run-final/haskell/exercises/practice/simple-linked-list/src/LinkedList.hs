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

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

-- | Returns the data stored in the head of the list
datum :: LinkedList a -> a
datum Nil = error "Cannot get datum from an empty list"
datum (Cons x _) = x

-- | Creates a LinkedList from a standard list
fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- | Checks if the list is empty
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

-- | Creates a new LinkedList with the given element as the head
new :: a -> LinkedList a -> LinkedList a
new = Cons

-- | Returns the tail of the list
next :: LinkedList a -> LinkedList a
next Nil = error "Cannot get next of an empty list"
next (Cons _ xs) = xs

-- | Returns an empty LinkedList
nil :: LinkedList a
nil = Nil

-- | Reverses the LinkedList
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go acc Nil = acc
    go acc (Cons x xs) = go (Cons x acc) xs

-- | Converts a LinkedList to a standard list
toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
