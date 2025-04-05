module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Prelude hiding (head, tail)

data Node a = Node {
    value :: a,
    prev :: IORef (Maybe (Node a)),
    next :: IORef (Maybe (Node a))
}

data Deque a = Deque {
    dequeHead :: IORef (Maybe (Node a)),
    dequeTail :: IORef (Maybe (Node a))
}

mkDeque :: IO (Deque a)
mkDeque = do
    h <- newIORef Nothing
    t <- newIORef Nothing
    return $ Deque h t

push :: Deque a -> a -> IO ()
push deque x = do
    newTail <- Node x <$> newIORef Nothing <*> newIORef Nothing
    oldTail <- readIORef (dequeTail deque)
    case oldTail of
        Nothing -> do  -- Empty deque
            writeIORef (dequeHead deque) (Just newTail)
            writeIORef (dequeTail deque) (Just newTail)
        Just node -> do
            writeIORef (next node) (Just newTail)
            writeIORef (prev newTail) (Just node)
            writeIORef (dequeTail deque) (Just newTail)

pop :: Deque a -> IO (Maybe a)
pop deque = do
    mNode <- readIORef (dequeTail deque)
    case mNode of
        Nothing -> return Nothing
        Just node -> do
            mPrev <- readIORef (prev node)
            case mPrev of
                Nothing -> do  -- Only one element
                    writeIORef (dequeHead deque) Nothing
                    writeIORef (dequeTail deque) Nothing
                Just prevNode -> do
                    writeIORef (next prevNode) Nothing
                    writeIORef (dequeTail deque) (Just prevNode)
            return $ Just (value node)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
    newHead <- Node x <$> newIORef Nothing <*> newIORef Nothing
    oldHead <- readIORef (dequeHead deque)
    case oldHead of
        Nothing -> do  -- Empty deque
            writeIORef (dequeHead deque) (Just newHead)
            writeIORef (dequeTail deque) (Just newHead)
        Just node -> do
            writeIORef (prev node) (Just newHead)
            writeIORef (next newHead) (Just node)
            writeIORef (dequeHead deque) (Just newHead)

shift :: Deque a -> IO (Maybe a)
shift deque = do
    mNode <- readIORef (dequeHead deque)
    case mNode of
        Nothing -> return Nothing
        Just node -> do
            mNext <- readIORef (next node)
            case mNext of
                Nothing -> do  -- Only one element
                    writeIORef (dequeHead deque) Nothing
                    writeIORef (dequeTail deque) Nothing
                Just nextNode -> do
                    writeIORef (prev nextNode) Nothing
                    writeIORef (dequeHead deque) (Just nextNode)
            return $ Just (value node)
