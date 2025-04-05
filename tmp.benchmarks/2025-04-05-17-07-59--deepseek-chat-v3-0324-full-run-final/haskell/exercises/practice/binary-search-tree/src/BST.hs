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
bstLeft (Node _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node value _ _) = Just value

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = empty
fromList xs = fromList' (sort xs)
  where
    fromList' [] = empty
    fromList' ys = Node (ys !! mid) 
                      (fromList' (take mid ys)) 
                      (fromList' (drop (mid + 1) ys))
      where mid = length ys `div` 2

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x <= y    = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node x left right) = toList left ++ [x] ++ toList right
