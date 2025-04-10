module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Doubly linked list node
data Node a = Node
  { prev :: IORef (Maybe (Node a))
  , value :: a
  , next :: IORef (Maybe (Node a))
  }

-- Deque with references to head and tail nodes and size
data Deque a = Deque
  { headRef :: IORef (Maybe (Node a))
  , tailRef :: IORef (Maybe (Node a))
  , sizeRef :: IORef Int
  }

-- Create an empty deque
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  s <- newIORef 0
  return $ Deque h t s

-- Add to the end (right)
push :: Deque a -> a -> IO ()
push dq x = do
  t <- readIORef (tailRef dq)
  nodePrevRef <- newIORef t
  nodeNextRef <- newIORef Nothing
  let node = Node nodePrevRef x nodeNextRef
  case t of
    Nothing -> do
      writeIORef (headRef dq) (Just node)
      writeIORef (tailRef dq) (Just node)
    Just oldTail -> do
      writeIORef (next oldTail) (Just node)
      writeIORef (tailRef dq) (Just node)
  modifyIORef' (sizeRef dq) (+1)

-- Add to the front (left)
unshift :: Deque a -> a -> IO ()
unshift dq x = do
  h <- readIORef (headRef dq)
  nodePrevRef <- newIORef Nothing
  nodeNextRef <- newIORef h
  let node = Node nodePrevRef x nodeNextRef
  case h of
    Nothing -> do
      writeIORef (headRef dq) (Just node)
      writeIORef (tailRef dq) (Just node)
    Just oldHead -> do
      writeIORef (prev oldHead) (Just node)
      writeIORef (headRef dq) (Just node)
  modifyIORef' (sizeRef dq) (+1)

-- Remove from the end (right)
pop :: Deque a -> IO (Maybe a)
pop dq = do
  t <- readIORef (tailRef dq)
  case t of
    Nothing -> return Nothing
    Just node -> do
      p <- readIORef (prev node)
      case p of
        Nothing -> do
          writeIORef (headRef dq) Nothing
          writeIORef (tailRef dq) Nothing
        Just prevNode -> do
          writeIORef (next prevNode) Nothing
          writeIORef (tailRef dq) (Just prevNode)
      modifyIORef' (sizeRef dq) (subtract 1)
      return (Just (value node))

-- Remove from the front (left)
shift :: Deque a -> IO (Maybe a)
shift dq = do
  h <- readIORef (headRef dq)
  case h of
    Nothing -> return Nothing
    Just node -> do
      n <- readIORef (next node)
      case n of
        Nothing -> do
          writeIORef (headRef dq) Nothing
          writeIORef (tailRef dq) Nothing
        Just nextNode -> do
          writeIORef (prev nextNode) Nothing
          writeIORef (headRef dq) (Just nextNode)
      modifyIORef' (sizeRef dq) (subtract 1)
      return (Just (value node))
