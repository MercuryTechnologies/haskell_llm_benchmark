module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.Char (chr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import System.Random (randomRIO)

-- Robot has a reference to its name that can be changed
newtype Robot = Robot (IORef String)

-- RunState keeps track of the names that are already in use
newtype RunState = RunState (Set.Set String)

-- Initialize with an empty set of used names
initialState :: RunState
initialState = RunState Set.empty

-- Generate a random letter (A-Z)
randomLetter :: IO Char
randomLetter = do
  n <- randomRIO (65, 90)  -- ASCII range for uppercase letters
  return (chr n)

-- Generate a random digit (0-9)
randomDigit :: IO Char
randomDigit = do
  n <- randomRIO (0, 9)
  return (head (show n))

-- Generate a random name (two letters followed by three digits)
randomName :: IO String
randomName = do
  l1 <- randomLetter
  l2 <- randomLetter
  d1 <- randomDigit
  d2 <- randomDigit
  d3 <- randomDigit
  return [l1, l2, d1, d2, d3]

-- Generate a unique random name
uniqueRandomName :: StateT RunState IO String
uniqueRandomName = do
  RunState usedNames <- get
  name <- liftIO randomName
  if name `Set.member` usedNames
    then uniqueRandomName  -- Retry if name is already used
    else do
      put (RunState (Set.insert name usedNames))
      return name

-- Create a new robot with a unique random name
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- uniqueRandomName
  nameRef <- liftIO (newIORef name)
  return (Robot nameRef)

-- Reset a robot's name to a new unique random name
resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
  name <- uniqueRandomName
  liftIO (writeIORef nameRef name)

-- Get a robot's name
robotName :: Robot -> IO String
robotName (Robot nameRef) = readIORef nameRef
