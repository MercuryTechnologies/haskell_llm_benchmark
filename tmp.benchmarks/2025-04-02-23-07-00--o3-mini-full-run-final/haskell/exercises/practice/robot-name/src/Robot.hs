module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, liftIO)
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe (unsafePerformIO)

-- Global registry for allocated names
{-# NOINLINE globalNames #-}
globalNames :: IORef (Set String)
globalNames = unsafePerformIO (newIORef Set.empty)

-- RunState is a dummy state since we use the global registry.
data RunState = RunState

initialState :: RunState
initialState = RunState

-- Robot holds a mutable reference to an optional name.
data Robot = Robot { nameRef :: IORef (Maybe String) }

-- Helper: Generate a candidate name in the format "AA000"
generateCandidate :: IO String
generateCandidate = do
  l1 <- randomRIO ('A', 'Z')
  l2 <- randomRIO ('A', 'Z')
  d1 <- randomRIO ('0', '9')
  d2 <- randomRIO ('0', '9')
  d3 <- randomRIO ('0', '9')
  return [l1, l2, d1, d2, d3]

-- Helper: Generate a unique name ensuring that every allocated name is unique.
generateUniqueName :: IO String
generateUniqueName = do
  candidate <- generateCandidate
  names <- readIORef globalNames
  if candidate `Set.member` names
    then generateUniqueName
    else do
      writeIORef globalNames (Set.insert candidate names)
      return candidate

mkRobot :: StateT RunState IO Robot
mkRobot = liftIO $ do
  ref <- newIORef Nothing
  return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = liftIO $ do
  mName <- readIORef ref
  case mName of
    Just name -> do
      names <- readIORef globalNames
      writeIORef globalNames (Set.delete name names)
      writeIORef ref Nothing
    Nothing ->
      return ()

robotName :: Robot -> IO String
robotName (Robot ref) = do
  mName <- readIORef ref
  case mName of
    Just name -> return name
    Nothing -> do
      name <- generateUniqueName
      writeIORef ref (Just name)
      return name
