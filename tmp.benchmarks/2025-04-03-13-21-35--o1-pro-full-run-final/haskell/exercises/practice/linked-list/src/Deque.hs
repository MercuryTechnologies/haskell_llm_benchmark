module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
  { val  :: a
  , prev :: IORef (Maybe (Node a))
  , next :: IORef (Maybe (Node a))
  }

data Deque a = Deque
  { front :: IORef (Maybe (Node a))
  , back  :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  frontRef <- newIORef Nothing
  backRef  <- newIORef Nothing
  return $ Deque frontRef backRef

pop :: Deque a -> IO (Maybe a)
pop (Deque frontRef backRef) = do
  maybeBackNode <- readIORef backRef
  case maybeBackNode of
    Nothing -> return Nothing
    Just node -> do
      let value = val node
      prevNode <- readIORef (prev node)
      case prevNode of
        Nothing -> do
          writeIORef frontRef Nothing
          writeIORef backRef Nothing
        Just p -> do
          writeIORef (next p) Nothing
          writeIORef backRef (Just p)
      return (Just value)

push :: Deque a -> a -> IO ()
push (Deque frontRef backRef) x = do
  maybeBackNode <- readIORef backRef
  case maybeBackNode of
    Nothing -> do
      newNode <- makeNode x
      writeIORef frontRef (Just newNode)
      writeIORef backRef (Just newNode)
    Just oldBack -> do
      newNode <- makeNode x
      writeIORef (prev newNode) (Just oldBack)
      writeIORef (next oldBack) (Just newNode)
      writeIORef backRef (Just newNode)
  where
    makeNode :: a -> IO (Node a)
    makeNode val = do
      pRef <- newIORef Nothing
      nRef <- newIORef Nothing
      return $ Node val pRef nRef

unshift :: Deque a -> a -> IO ()
unshift (Deque frontRef backRef) x = do
  maybeFrontNode <- readIORef frontRef
  case maybeFrontNode of
    Nothing -> do
      newNode <- makeNode x
      writeIORef frontRef (Just newNode)
      writeIORef backRef (Just newNode)
    Just oldFront -> do
      newNode <- makeNode x
      writeIORef (next newNode) (Just oldFront)
      writeIORef (prev oldFront) (Just newNode)
      writeIORef frontRef (Just newNode)
  where
    makeNode :: a -> IO (Node a)
    makeNode val = do
      pRef <- newIORef Nothing
      nRef <- newIORef Nothing
      return $ Node val pRef nRef

shift :: Deque a -> IO (Maybe a)
shift (Deque frontRef backRef) = do
  maybeFrontNode <- readIORef frontRef
  case maybeFrontNode of
    Nothing -> return Nothing
    Just node -> do
      let value = val node
      nextNode <- readIORef (next node)
      case nextNode of
        Nothing -> do
          writeIORef frontRef Nothing
          writeIORef backRef Nothing
        Just n -> do
          writeIORef (prev n) Nothing
          writeIORef frontRef (Just n)
      return (Just value)
