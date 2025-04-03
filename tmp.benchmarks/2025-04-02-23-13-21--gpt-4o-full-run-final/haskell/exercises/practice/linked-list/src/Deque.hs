module Deque (Deque, mkDeque, pop, push, shift, unshift) where

data Node a = Node
  { value :: a
  , prev  :: Maybe (Node a)
  , next  :: Maybe (Node a)
  }

data Deque a = Deque
  { head :: Maybe (Node a)
  , tail :: Maybe (Node a)
  }

mkDeque :: IO (Deque a)
mkDeque = return $ Deque Nothing Nothing

pop :: Deque a -> IO (Maybe a)
pop (Deque Nothing _) = return Nothing
pop (Deque (Just node) tail) = do
  let newHead = next node
  case newHead of
    Just n  -> return $ Just (value node)
    Nothing -> return $ Just (value node)

push :: Deque a -> a -> IO ()
push (Deque head Nothing) x = do
  let newNode = Node x Nothing Nothing
  head <- return $ Just newNode
  tail <- return $ Just newNode
  return ()
push (Deque head (Just tailNode)) x = do
  let newNode = Node x (Just tailNode) Nothing
  tailNode <- return $ Just newNode
  return ()

unshift :: Deque a -> a -> IO ()
unshift (Deque Nothing tail) x = do
  let newNode = Node x Nothing Nothing
  head <- return $ Just newNode
  tail <- return $ Just newNode
  return ()
unshift (Deque (Just headNode) tail) x = do
  let newNode = Node x Nothing (Just headNode)
  headNode <- return $ Just newNode
  return ()

shift :: Deque a -> IO (Maybe a)
shift (Deque _ Nothing) = return Nothing
shift (Deque head (Just node)) = do
  let newTail = prev node
  case newTail of
    Just n  -> return $ Just (value node)
    Nothing -> return $ Just (value node)
