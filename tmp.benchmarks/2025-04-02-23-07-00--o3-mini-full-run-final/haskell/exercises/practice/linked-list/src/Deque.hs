module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
  { prev :: IORef (Maybe (Node a))
  , value :: a
  , next :: IORef (Maybe (Node a))
  }

data Deque a = Deque
  { front :: IORef (Maybe (Node a))
  , back  :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  frontRef <- newIORef Nothing
  backRef <- newIORef Nothing
  return $ Deque frontRef backRef

push :: Deque a -> a -> IO ()
push deque x = do
  mOldBack <- readIORef (back deque)
  prevRef <- newIORef mOldBack
  nextRef <- newIORef Nothing
  let newNode = Node prevRef x nextRef
  case mOldBack of
    Nothing -> writeIORef (front deque) (Just newNode)
    Just node -> writeIORef (next node) (Just newNode)
  writeIORef (back deque) (Just newNode)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  mOldFront <- readIORef (front deque)
  prevRef <- newIORef Nothing
  nextRef <- newIORef mOldFront
  let newNode = Node prevRef x nextRef
  case mOldFront of
    Nothing -> writeIORef (back deque) (Just newNode)
    Just node -> writeIORef (prev node) (Just newNode)
  writeIORef (front deque) (Just newNode)

pop :: Deque a -> IO (Maybe a)
pop deque = do
  mNode <- readIORef (back deque)
  case mNode of
    Nothing -> return Nothing
    Just node -> do
      mPrev <- readIORef (prev node)
      case mPrev of
        Nothing -> do
          writeIORef (front deque) Nothing
          writeIORef (back deque) Nothing
        Just prevNode -> do
          writeIORef (back deque) (Just prevNode)
          writeIORef (next prevNode) Nothing
      return (Just (value node))

shift :: Deque a -> IO (Maybe a)
shift deque = do
  mNode <- readIORef (front deque)
  case mNode of
    Nothing -> return Nothing
    Just node -> do
      mNext <- readIORef (next node)
      case mNext of
        Nothing -> do
          writeIORef (front deque) Nothing
          writeIORef (back deque) Nothing
        Just nextNode -> do
          writeIORef (front deque) (Just nextNode)
          writeIORef (prev nextNode) Nothing
      return (Just (value node))
