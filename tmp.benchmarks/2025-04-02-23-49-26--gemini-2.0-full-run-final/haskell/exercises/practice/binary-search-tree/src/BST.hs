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
bstLeft Empty = Nothing
bstLeft (Node _ left _) = case left of
    Empty -> Nothing
    _ -> Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ right) = case right of
    Empty -> Nothing
    _ -> Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node value _ _) = Just value

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList xs = foldl (\acc x -> insert x acc) empty xs

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node value left right)
    | x <= value = Node value (insert x left) right
    | otherwise = Node value left (insert x right)

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList tree = toListHelper tree []

toListHelper :: BST a -> [a] -> [a]
toListHelper Empty acc = acc
toListHelper (Node value left right) acc = toListHelper left (value : toListHelper right acc)
