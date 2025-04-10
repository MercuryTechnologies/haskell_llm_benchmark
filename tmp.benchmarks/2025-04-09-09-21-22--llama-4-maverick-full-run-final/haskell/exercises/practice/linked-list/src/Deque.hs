module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Monad (when)

data Node a = Node { nodeValue :: a, prevNode :: Maybe (Node a), nextNode :: Maybe (Node a) }

data Deque a = Deque { firstNode :: Maybe (Node a), lastNode :: Maybe (Node a), dequeSize :: Int }

mkDeque :: IO (Deque a)
mkDeque = return $ Deque Nothing Nothing 0

pop :: Deque a -> IO (Maybe a)
pop deque = do
  let lastN = lastNode deque
  case lastN of
    Nothing -> return Nothing
    Just node -> do
      let newLastN = prevNode node
      case newLastN of
        Nothing -> return $ Just $ nodeValue node
        Just n -> do
          n' <- return $ n { nextNode = Nothing }
          let newDeque = Deque (firstNode deque) (Just n') (dequeSize deque - 1)
          return $ Just $ nodeValue node

push :: Deque a -> a -> IO ()
push deque x = do
  let newNode = Node x (lastNode deque) Nothing
  case lastNode deque of
    Nothing -> return ()
    Just lastN -> do
      lastN' <- return $ lastN { nextNode = Just newNode }
      return ()
  when (isNothing (firstNode deque)) $ do
    let newFirst = Just newNode
    return ()
  let newLastN = Just newNode
  let newDeque = case lastNode deque of
                   Nothing -> Deque newLastN newLastN 1
                   Just _ -> Deque (firstNode deque) newLastN (dequeSize deque + 1)
  return ()

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  let newNode = Node x Nothing (firstNode deque)
  case firstNode deque of
    Nothing -> return ()
    Just firstN -> do
      firstN' <- return $ firstN { prevNode = Just newNode }
      return ()
  when (isNothing (lastNode deque)) $ do
    let newLast = Just newNode
    return ()
  let newFirstN = Just newNode
  let newDeque = Deque newFirstN (lastNode deque) (dequeSize deque + 1)
  return ()

shift :: Deque a -> IO (Maybe a)
shift deque = do
  let firstN = firstNode deque
  case firstN of
    Nothing -> return Nothing
    Just node -> do
      let newFirstN = nextNode node
      case newFirstN of
        Nothing -> return $ Just $ nodeValue node
        Just n -> do
          n' <- return $ n { prevNode = Nothing }
          let newDeque = Deque (Just n') (lastNode deque) (dequeSize deque - 1)
          return $ Just $ nodeValue node

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False
