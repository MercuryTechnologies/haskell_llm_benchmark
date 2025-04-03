module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- | A node in the doubly linked list
data Node a = Node
  { nodeValue :: a
  , nodePrev  :: IORef (Maybe (Node a))
  , nodeNext  :: IORef (Maybe (Node a))
  }

-- | The Deque, holding references to the front and the back node
data Deque a = Deque
  { front :: IORef (Maybe (Node a))
  , back  :: IORef (Maybe (Node a))
  }

-- | Create a new empty Deque
mkDeque :: IO (Deque a)
mkDeque = do
  f <- newIORef Nothing
  b <- newIORef Nothing
  return (Deque f b)

-- | Push a value onto the back of the Deque
push :: Deque a -> a -> IO ()
push (Deque f b) value = do
  currentBack <- readIORef b
  newNodePrevRef <- newIORef currentBack
  newNodeNextRef <- newIORef Nothing
  let newNode = Node value newNodePrevRef newNodeNextRef
  case currentBack of
    Nothing -> do
      -- The Deque is empty
      writeIORef f (Just newNode)
      writeIORef b (Just newNode)
    Just oldBack -> do
      -- Link old back node to the new node
      writeIORef (nodeNext oldBack) (Just newNode)
      writeIORef b (Just newNode)

-- | Pop a value from the back of the Deque
pop :: Deque a -> IO (Maybe a)
pop (Deque f b) = do
  currentBack <- readIORef b
  case currentBack of
    Nothing ->
      -- Deque is empty
      return Nothing
    Just node -> do
      let val = nodeValue node
      prevNode <- readIORef (nodePrev node)
      case prevNode of
        Nothing -> do
          -- This was the only node in the Deque
          writeIORef f Nothing
          writeIORef b Nothing
        Just pn -> do
          -- Remove the last node
          writeIORef (nodeNext pn) Nothing
          writeIORef b (Just pn)
      return (Just val)

-- | Unshift a value onto the front of the Deque
unshift :: Deque a -> a -> IO ()
unshift (Deque f b) value = do
  currentFront <- readIORef f
  newNodePrevRef <- newIORef Nothing
  newNodeNextRef <- newIORef currentFront
  let newNode = Node value newNodePrevRef newNodeNextRef
  case currentFront of
    Nothing -> do
      -- The Deque is empty
      writeIORef f (Just newNode)
      writeIORef b (Just newNode)
    Just oldFront -> do
      -- Link old front node to the new node
      writeIORef (nodePrev oldFront) (Just newNode)
      writeIORef f (Just newNode)

-- | Shift (remove) a value from the front of the Deque
shift :: Deque a -> IO (Maybe a)
shift (Deque f b) = do
  currentFront <- readIORef f
  case currentFront of
    Nothing ->
      -- Deque is empty
      return Nothing
    Just node -> do
      let val = nodeValue node
      nextNode <- readIORef (nodeNext node)
      case nextNode of
        Nothing -> do
          -- This was the only node in the Deque
          writeIORef f Nothing
          writeIORef b Nothing
        Just nn -> do
          -- Remove the front node and update the next node
          writeIORef (nodePrev nn) Nothing
          writeIORef f (Just nn)
      return (Just val)
