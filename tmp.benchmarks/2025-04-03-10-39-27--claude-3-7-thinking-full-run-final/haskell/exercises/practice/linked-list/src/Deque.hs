module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- A node in our doubly linked list
data Node a = Node
    { value :: a
    , prev :: IORef (Maybe (Node a))
    , next :: IORef (Maybe (Node a))
    }

-- Our Deque keeps track of the first and last nodes in the train route
data Deque a = Deque
    { first :: IORef (Maybe (Node a))
    , last :: IORef (Maybe (Node a))
    }

-- Create a new empty deque (an empty train route)
mkDeque :: IO (Deque a)
mkDeque = do
    firstRef <- newIORef Nothing
    lastRef <- newIORef Nothing
    return $ Deque firstRef lastRef

-- Remove and return the last station from the route
pop :: Deque a -> IO (Maybe a)
pop (Deque firstRef lastRef) = do
    lastNodeMaybe <- readIORef lastRef
    case lastNodeMaybe of
        Nothing -> return Nothing  -- Empty route
        Just lastNode -> do
            prevNodeMaybe <- readIORef (prev lastNode)
            case prevNodeMaybe of
                Nothing -> do
                    -- Only one station in the route
                    writeIORef firstRef Nothing
                    writeIORef lastRef Nothing
                Just prevNode -> do
                    -- Update the last reference
                    writeIORef lastRef (Just prevNode)
                    -- Remove the reference to the removed station
                    writeIORef (next prevNode) Nothing
            return $ Just (value lastNode)

-- Add a station to the end of the route
push :: Deque a -> a -> IO ()
push (Deque firstRef lastRef) x = do
    newPrevRef <- newIORef Nothing
    newNextRef <- newIORef Nothing
    let newNode = Node x newPrevRef newNextRef
    
    lastNodeMaybe <- readIORef lastRef
    case lastNodeMaybe of
        Nothing -> do
            -- Empty route
            writeIORef firstRef (Just newNode)
            writeIORef lastRef (Just newNode)
        Just lastNode -> do
            -- Update the last station's next reference
            writeIORef (next lastNode) (Just newNode)
            -- Update the new station's prev reference
            writeIORef newPrevRef (Just lastNode)
            -- Update the last reference
            writeIORef lastRef (Just newNode)

-- Add a station to the beginning of the route
unshift :: Deque a -> a -> IO ()
unshift (Deque firstRef lastRef) x = do
    newPrevRef <- newIORef Nothing
    newNextRef <- newIORef Nothing
    let newNode = Node x newPrevRef newNextRef
    
    firstNodeMaybe <- readIORef firstRef
    case firstNodeMaybe of
        Nothing -> do
            -- Empty route
            writeIORef firstRef (Just newNode)
            writeIORef lastRef (Just newNode)
        Just firstNode -> do
            -- Update the first station's prev reference
            writeIORef (prev firstNode) (Just newNode)
            -- Update the new station's next reference
            writeIORef newNextRef (Just firstNode)
            -- Update the first reference
            writeIORef firstRef (Just newNode)

-- Remove and return the first station from the route
shift :: Deque a -> IO (Maybe a)
shift (Deque firstRef lastRef) = do
    firstNodeMaybe <- readIORef firstRef
    case firstNodeMaybe of
        Nothing -> return Nothing  -- Empty route
        Just firstNode -> do
            nextNodeMaybe <- readIORef (next firstNode)
            case nextNodeMaybe of
                Nothing -> do
                    -- Only one station in the route
                    writeIORef firstRef Nothing
                    writeIORef lastRef Nothing
                Just nextNode -> do
                    -- Update the first reference
                    writeIORef firstRef (Just nextNode)
                    -- Remove the reference to the removed station
                    writeIORef (prev nextNode) Nothing
            return $ Just (value firstNode)
