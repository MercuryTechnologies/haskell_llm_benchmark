module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node _ left _) = Just left
bstLeft Empty = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ right) = Just right
bstRight Empty = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node value _ _) = Just value
bstValue Empty = Nothing

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldr insert empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node value left right)
    | x < value = Node value (insert x left) right
    | x > value = Node value left (insert x right)
    | otherwise = Node value left right  -- Allow duplicates by returning the existing node

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node value left right) = toList left ++ [value] ++ toList right
