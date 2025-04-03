module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO) -- Moved import here

-- Node in the doubly linked list
data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (IORef (Node a))) -- Reference to the previous node ref
  , next  :: IORef (Maybe (IORef (Node a))) -- Reference to the next node ref
  }

-- The Deque structure holding references to the head and tail node references
data Deque a = Deque
  { head :: IORef (Maybe (IORef (Node a))) -- Reference to the head node ref
  , tail :: IORef (Maybe (IORef (Node a))) -- Reference to the tail node ref
  }

-- Creates a new empty Deque
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  return $ Deque h t

-- Helper to create a new node
newNode :: a -> IO (IORef (Node a))
newNode val = do
  p <- newIORef Nothing
  n <- newIORef Nothing
  newIORef $ Node val p n

-- Pushes a value onto the end (tail) of the deque
push :: Deque a -> a -> IO ()
push deque val = do
  newNodeRef <- newNode val
  mOldTailRef <- readIORef (tail deque)
  case mOldTailRef of
    Nothing -> do -- Deque was empty
      writeIORef (head deque) (Just newNodeRef)
      writeIORef (tail deque) (Just newNodeRef)
    Just oldTailRef -> do -- Deque was not empty
      newNode <- readIORef newNodeRef -- Read the new node to access its fields
      oldTailNode <- readIORef oldTailRef -- Read the old tail node
      -- Update new node's prev
      writeIORef (prev newNode) (Just oldTailRef)
      -- Update old tail's next
      writeIORef (next oldTailNode) (Just newNodeRef)
      -- Update deque's tail
      writeIORef (tail deque) (Just newNodeRef)

-- Pops a value from the end (tail) of the deque
pop :: Deque a -> IO (Maybe a)
pop deque = do
  mTailRef <- readIORef (tail deque)
  case mTailRef of
    Nothing -> return Nothing -- Deque is empty
    Just tailRef -> do
      tailNode <- readIORef tailRef
      let val = value tailNode
      mPrevRef <- readIORef (prev tailNode)
      writeIORef (tail deque) mPrevRef -- Update deque's tail to the previous node (or Nothing)
      case mPrevRef of
        Nothing -> -- Deque had only one element
          writeIORef (head deque) Nothing -- Also clear the head
        Just prevRef -> do -- Deque had more than one element
          -- Update the new tail's next pointer to Nothing
          prevNode <- readIORef prevRef
          writeIORef (next prevNode) Nothing
      return (Just val)

-- Pushes a value onto the beginning (head) of the deque
unshift :: Deque a -> a -> IO ()
unshift deque val = do
  newNodeRef <- newNode val
  mOldHeadRef <- readIORef (head deque)
  case mOldHeadRef of
    Nothing -> do -- Deque was empty
      writeIORef (head deque) (Just newNodeRef)
      writeIORef (tail deque) (Just newNodeRef)
    Just oldHeadRef -> do -- Deque was not empty
      newNode <- readIORef newNodeRef -- Read the new node
      oldHeadNode <- readIORef oldHeadRef -- Read the old head node
      -- Update new node's next
      writeIORef (next newNode) (Just oldHeadRef)
      -- Update old head's prev
      writeIORef (prev oldHeadNode) (Just newNodeRef)
      -- Update deque's head
      writeIORef (head deque) (Just newNodeRef)

-- Pops a value from the beginning (head) of the deque
shift :: Deque a -> IO (Maybe a)
shift deque = do
  mHeadRef <- readIORef (head deque)
  case mHeadRef of
    Nothing -> return Nothing -- Deque is empty
    Just headRef -> do
      headNode <- readIORef headRef
      let val = value headNode
      mNextRef <- readIORef (next headNode)
      writeIORef (head deque) mNextRef -- Update deque's head to the next node (or Nothing)
      case mNextRef of
        Nothing -> -- Deque had only one element
          writeIORef (tail deque) Nothing -- Also clear the tail
        Just nextRef -> do -- Deque had more than one element
          -- Update the new head's prev pointer to Nothing
          nextNode <- readIORef nextRef
          writeIORef (prev nextNode) Nothing
      return (Just val)

-- Helper function to simplify reading IORef content when we know it's not empty.
-- Note: This is unsafe if used incorrectly, but simplifies logic within functions
-- where we've already checked for Nothing.
-- This function is problematic and likely causing issues. Avoid using unsafePerformIO here.
-- It's better to read the IORef within the IO monad where needed.
-- unsafeReadIORef :: IORef a -> a
-- unsafeReadIORef ref = unsafePerformIO (readIORef ref)

-- Removed the unsafeReadIORef helper and updated push/unshift to read IORefs directly.
