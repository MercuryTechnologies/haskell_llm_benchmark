module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Node a = Node {
  nodeValue :: a,
  nodePrev :: IORef (Maybe (Node a)),
  nodeNext :: IORef (Maybe (Node a))
}

data Deque a = Deque {
  dequeHead :: IORef (Maybe (Node a)),
  dequeTail :: IORef (Maybe (Node a))
}

mkDeque :: IO (Deque a)
mkDeque = do
  headRef <- newIORef Nothing
  tailRef <- newIORef Nothing
  return (Deque headRef tailRef)

push :: Deque a -> a -> IO ()
push (Deque headRef tailRef) x = do
  newNodeRef <- newIORef $ Node x (unsafePerformIO $ newIORef Nothing) (unsafePerformIO $ newIORef Nothing)
  newNode <- readIORef newNodeRef
  oldHead <- readIORef headRef
  case oldHead of
    Nothing -> do
      writeIORef headRef (Just newNode)
      writeIORef tailRef (Just newNode)
    Just oldHeadNode -> do
      writeIORef (nodeNext newNode) (Just oldHeadNode)
      writeIORef (nodePrev oldHeadNode) (Just newNode)
      writeIORef headRef (Just newNode)

pop :: Deque a -> IO (Maybe a)
pop (Deque headRef tailRef) = do
  oldHead <- readIORef headRef
  case oldHead of
    Nothing -> return Nothing
    Just oldHeadNode -> do
      let value = nodeValue oldHeadNode
      nextMaybe <- readIORef (nodeNext oldHeadNode)
      case nextMaybe of
        Nothing -> do
          writeIORef headRef Nothing
          writeIORef tailRef Nothing
        Just nextNode -> do
          writeIORef headRef (Just nextNode)
          writeIORef (nodePrev nextNode) (unsafePerformIO $ newIORef Nothing)
      return (Just value)

unshift :: Deque a -> a -> IO ()
unshift (Deque headRef tailRef) x = do
  newNodeRef <- newIORef $ Node x (unsafePerformIO $ newIORef Nothing) (unsafePerformIO $ newIORef Nothing)
  newNode <- readIORef newNodeRef
  oldTail <- readIORef tailRef
  case oldTail of
    Nothing -> do
      writeIORef headRef (Just newNode)
      writeIORef tailRef (Just newNode)
    Just oldTailNode -> do
      writeIORef (nodePrev newNode) (Just oldTailNode)
      writeIORef (nodeNext oldTailNode) (Just newNode)
      writeIORef tailRef (Just newNode)

shift :: Deque a -> IO (Maybe a)
shift (Deque headRef tailRef) = do
  oldTail <- readIORef tailRef
  case oldTail of
    Nothing -> return Nothing
    Just oldTailNode -> do
      let value = nodeValue oldTailNode
      prevMaybe <- readIORef (nodePrev oldTailNode)
      case prevMaybe of
        Nothing -> do
          writeIORef headRef Nothing
          writeIORef tailRef Nothing
        Just prevNode -> do
          writeIORef tailRef (Just prevNode)
          writeIORef (nodeNext prevNode) (unsafePerformIO $ newIORef Nothing)
      return (Just value)
