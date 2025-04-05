module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
  { prev :: IORef (Maybe (Node a))
  , value :: a
  , next :: IORef (Maybe (Node a))
  }

data Deque a = Deque
  { headRef :: IORef (Maybe (Node a))
  , tailRef :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  headPtr <- newIORef Nothing
  tailPtr <- newIORef Nothing
  return $ Deque headPtr tailPtr

push :: Deque a -> a -> IO ()
push deque x = do
  newPrev <- newIORef Nothing
  newNext <- newIORef Nothing
  let newNode = Node newPrev x newNext
  tailNodeM <- readIORef (tailRef deque)
  case tailNodeM of
    Nothing -> do
      -- empty list
      writeIORef (headRef deque) (Just newNode)
      writeIORef (tailRef deque) (Just newNode)
    Just oldTail -> do
      writeIORef (prev newNode) (Just oldTail)
      writeIORef (next oldTail) (Just newNode)
      writeIORef (tailRef deque) (Just newNode)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  newPrev <- newIORef Nothing
  newNext <- newIORef Nothing
  let newNode = Node newPrev x newNext
  headNodeM <- readIORef (headRef deque)
  case headNodeM of
    Nothing -> do
      -- empty list
      writeIORef (headRef deque) (Just newNode)
      writeIORef (tailRef deque) (Just newNode)
    Just oldHead -> do
      writeIORef (next newNode) (Just oldHead)
      writeIORef (prev oldHead) (Just newNode)
      writeIORef (headRef deque) (Just newNode)

pop :: Deque a -> IO (Maybe a)
pop deque = do
  tailNodeM <- readIORef (tailRef deque)
  case tailNodeM of
    Nothing -> return Nothing
    Just node -> do
      prevNodeM <- readIORef (prev node)
      case prevNodeM of
        Nothing -> do
          -- only one element
          writeIORef (headRef deque) Nothing
          writeIORef (tailRef deque) Nothing
        Just prevNode -> do
          writeIORef (next prevNode) Nothing
          writeIORef (tailRef deque) (Just prevNode)
      return (Just (value node))

shift :: Deque a -> IO (Maybe a)
shift deque = do
  headNodeM <- readIORef (headRef deque)
  case headNodeM of
    Nothing -> return Nothing
    Just node -> do
      nextNodeM <- readIORef (next node)
      case nextNodeM of
        Nothing -> do
          -- only one element
          writeIORef (headRef deque) Nothing
          writeIORef (tailRef deque) Nothing
        Just nextNode -> do
          writeIORef (prev nextNode) Nothing
          writeIORef (headRef deque) (Just nextNode)
      return (Just (value node))
