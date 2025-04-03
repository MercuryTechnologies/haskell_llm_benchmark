module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Prelude hiding (head, tail) -- Hide Prelude's head and tail
import Data.IORef
import Control.Monad (when)

-- Node type for the doubly linked list
data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

-- Deque type holding references to head and tail nodes
data Deque a = Deque
  { head :: IORef (Maybe (Node a))
  , tail :: IORef (Maybe (Node a))
  }

-- Creates a new empty Deque
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  return (Deque h t)

-- Creates a new Node
newNode :: a -> IO (Node a)
newNode x = do
  p <- newIORef Nothing
  n <- newIORef Nothing
  return (Node x p n)

-- Adds an element to the end (tail) of the Deque
push :: Deque a -> a -> IO ()
push deque x = do
  node <- newNode x
  mOldTail <- readIORef (tail deque) -- Now refers unambiguously to the record field
  case mOldTail of
    Nothing -> do -- Empty deque
      writeIORef (head deque) (Just node) -- Now refers unambiguously to the record field
      writeIORef (tail deque) (Just node) -- Now refers unambiguously to the record field
    Just oldTail -> do -- Non-empty deque
      writeIORef (prev node) (Just oldTail)
      writeIORef (next oldTail) (Just node)
      writeIORef (tail deque) (Just node) -- Now refers unambiguously to the record field

-- Removes and returns an element from the end (tail) of the Deque
pop :: Deque a -> IO (Maybe a)
pop deque = do
  mOldTail <- readIORef (tail deque) -- Now refers unambiguously to the record field
  case mOldTail of
    Nothing -> return Nothing -- Empty deque
    Just oldTail -> do
      let val = value oldTail
      mPrevNode <- readIORef (prev oldTail)
      writeIORef (tail deque) mPrevNode -- Now refers unambiguously to the record field
      case mPrevNode of
        Nothing -> -- Deque becomes empty
          writeIORef (head deque) Nothing -- Now refers unambiguously to the record field
        Just prevNode -> -- Deque still has elements
          writeIORef (next prevNode) Nothing
      return (Just val)

-- Adds an element to the beginning (head) of the Deque
unshift :: Deque a -> a -> IO ()
unshift deque x = do
  node <- newNode x
  mOldHead <- readIORef (head deque) -- Now refers unambiguously to the record field
  case mOldHead of
    Nothing -> do -- Empty deque
      writeIORef (head deque) (Just node) -- Now refers unambiguously to the record field
      writeIORef (tail deque) (Just node) -- Now refers unambiguously to the record field
    Just oldHead -> do -- Non-empty deque
      writeIORef (next node) (Just oldHead)
      writeIORef (prev oldHead) (Just node)
      writeIORef (head deque) (Just node) -- Now refers unambiguously to the record field

-- Removes and returns an element from the beginning (head) of the Deque
shift :: Deque a -> IO (Maybe a)
shift deque = do
  mOldHead <- readIORef (head deque) -- Now refers unambiguously to the record field
  case mOldHead of
    Nothing -> return Nothing -- Empty deque
    Just oldHead -> do
      let val = value oldHead
      mNextNode <- readIORef (next oldHead)
      writeIORef (head deque) mNextNode -- Now refers unambiguously to the record field
      case mNextNode of
        Nothing -> -- Deque becomes empty
          writeIORef (tail deque) Nothing -- Now refers unambiguously to the record field
        Just nextNode -> -- Deque still has elements
          writeIORef (prev nextNode) Nothing
      return (Just val)
