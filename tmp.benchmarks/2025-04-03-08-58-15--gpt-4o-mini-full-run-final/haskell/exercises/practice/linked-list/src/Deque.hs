module Deque (Deque, mkDeque, pop, push, shift, unshift) where

data Node a = Node {
    value :: a,
    prev :: Maybe (Node a),
    next :: Maybe (Node a)
}

data Deque a = Deque {
    front :: Maybe (Node a),
    back :: Maybe (Node a),
    size :: Int
}

mkDeque :: IO (Deque a)
mkDeque = return $ Deque Nothing Nothing 0

pop :: Deque a -> IO (Maybe a)
pop (Deque Nothing _ _) = return Nothing
pop (Deque (Just frontNode) backNode s) = do
    let newFront = next frontNode
    let newSize = s - 1
    return $ Just (value frontNode)

push :: Deque a -> a -> IO (Deque a)
push (Deque frontNode backNode s) x = do
    let newNode = Node x backNode Nothing
    case backNode of
        Nothing -> return $ Deque (Just newNode) (Just newNode) (s + 1)
        Just backNode' -> do
            let updatedBack = backNode' { next = Just newNode }
            return $ Deque frontNode (Just newNode) (s + 1)

unshift :: Deque a -> a -> IO (Deque a)
unshift (Deque frontNode backNode s) x = do
    let newNode = Node x Nothing frontNode
    case frontNode of
        Nothing -> return $ Deque (Just newNode) (Just newNode) (s + 1)
        Just frontNode' -> do
            let updatedFront = frontNode' { prev = Just newNode }
            return $ Deque (Just newNode) backNode (s + 1)

shift :: Deque a -> IO (Maybe a)
shift (Deque Nothing _ _) = return Nothing
shift (Deque (Just frontNode) backNode s) = do
    let newFront = prev frontNode
    let newSize = s - 1
    return $ Just (value frontNode)
