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

data BST a = Empty
           | Node { left  :: BST a
                  , value :: a
                  , right :: BST a
                  }
           deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node l _ _) = Just l
bstLeft Empty = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ r) = Just r
bstRight Empty = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node _ v _) = Just v
bstValue Empty        = Nothing

empty :: BST a
empty = Empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node l v r)
    | x <= v    = Node (insert x l) v r
    | otherwise = Node l v (insert x r)

singleton :: a -> BST a
singleton x = Node Empty x Empty

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node l v r) = toList l ++ [v] ++ toList r
