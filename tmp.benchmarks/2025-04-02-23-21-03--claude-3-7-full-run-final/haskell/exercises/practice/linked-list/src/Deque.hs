module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Node in our doubly linked list
data Node a = Node
  { value :: a,
    prev :: IORef (Maybe (Node a)),
    next :: IORef (Maybe (Node a))
  }

-- Deque is represented by references to the first and last nodes
data Deque a = Deque
  { first :: IORef (Maybe (Node a)),
    last :: IORef (Maybe (Node a))
  }

-- Create a new empty deque
mkDeque :: IO (Deque a)
mkDeque = do
  firstRef <- newIORef Nothing
  lastRef <- newIORef Nothing
  return $ Deque firstRef lastRef

-- Remove from the end of the deque and return the value
pop :: Deque a -> IO (Maybe a)
pop (Deque firstRef lastRef) = do
  lastNode <- readIORef lastRef
  case lastNode of
    Nothing -> return Nothing
    Just node -> do
      prevNode <- readIORef (prev node)
      case prevNode of
        Nothing -> do
          -- Deque will be empty after this pop
          writeIORef firstRef Nothing
          writeIORef lastRef Nothing
        Just prevN -> do
          -- Update the last node to be the previous node
          writeIORef lastRef (Just prevN)
          writeIORef (next prevN) Nothing
      return $ Just (value node)

-- Add a value to the end of the deque
push :: Deque a -> a -> IO ()
push (Deque firstRef lastRef) x = do
  newNodePrevRef <- newIORef Nothing
  newNodeNextRef <- newIORef Nothing
  let newNode = Node x newNodePrevRef newNodeNextRef
  
  lastNode <- readIORef lastRef
  case lastNode of
    Nothing -> do
      -- Deque is empty, new node becomes both first and last
      writeIORef firstRef (Just newNode)
      writeIORef lastRef (Just newNode)
    Just node -> do
      -- Link the new node to the current last node
      writeIORef newNodePrevRef (Just node)
      writeIORef (next node) (Just newNode)
      writeIORef lastRef (Just newNode)

-- Add a value to the beginning of the deque
unshift :: Deque a -> a -> IO ()
unshift (Deque firstRef lastRef) x = do
  newNodePrevRef <- newIORef Nothing
  newNodeNextRef <- newIORef Nothing
  let newNode = Node x newNodePrevRef newNodeNextRef
  
  firstNode <- readIORef firstRef
  case firstNode of
    Nothing -> do
      -- Deque is empty, new node becomes both first and last
      writeIORef firstRef (Just newNode)
      writeIORef lastRef (Just newNode)
    Just node -> do
      -- Link the new node to the current first node
      writeIORef newNodeNextRef (Just node)
      writeIORef (prev node) (Just newNode)
      writeIORef firstRef (Just newNode)

-- Remove from the beginning of the deque and return the value
shift :: Deque a -> IO (Maybe a)
shift (Deque firstRef lastRef) = do
  firstNode <- readIORef firstRef
  case firstNode of
    Nothing -> return Nothing
    Just node -> do
      nextNode <- readIORef (next node)
      case nextNode of
        Nothing -> do
          -- Deque will be empty after this shift
          writeIORef firstRef Nothing
          writeIORef lastRef Nothing
        Just nextN -> do
          -- Update the first node to be the next node
          writeIORef firstRef (Just nextN)
          writeIORef (prev nextN) Nothing
      return $ Just (value node)
