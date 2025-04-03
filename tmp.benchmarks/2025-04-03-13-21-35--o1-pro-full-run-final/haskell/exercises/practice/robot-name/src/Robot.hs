module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, modify, liftIO)
import System.Random       (randomRIO)
import Data.IORef          (IORef, newIORef, readIORef, writeIORef)

-- | The 'Robot' data holds a reference to the robot's current name.
data Robot = Robot
  { nameRef :: IORef String
  }

-- | 'RunState' holds a list of all currently used names.
--   We will add new robot names here and remove old ones if a robot is reset.
data RunState = RunState
  { usedNames :: [String]
  }

-- | The initial state has no used names.
initialState :: RunState
initialState = RunState
  { usedNames = []
  }

-- | Create a fresh robot and assign it a unique name right away.
--   We update 'usedNames' so that no two robots overlap names.
mkRobot :: StateT RunState IO Robot
mkRobot = do
  newName <- getUniqueName
  robot <- liftIO $ Robot <$> newIORef newName
  return robot

-- | Reset an existing robot to factory settings by assigning it a new name.
--   We remove the old name from 'usedNames' and store the new one.
resetName :: Robot -> StateT RunState IO ()
resetName robot = do
  oldName <- liftIO (readIORef (nameRef robot))
  modify (\st -> st { usedNames = filter (/= oldName) (usedNames st) })
  newName <- getUniqueName
  liftIO $ writeIORef (nameRef robot) newName

-- | Return the name of a given robot.
robotName :: Robot -> IO String
robotName robot = readIORef (nameRef robot)

-- INTERNALS

-- | Generate a random name that is guaranteed not to be used yet.
getUniqueName :: StateT RunState IO String
getUniqueName = do
  st <- get
  candidate <- liftIO randomRobotName
  if candidate `elem` usedNames st
    then getUniqueName
    else do
      put st { usedNames = candidate : usedNames st }
      return candidate

-- | Build a random robot name of the form "AB123".
randomRobotName :: IO String
randomRobotName = do
  let randLetter = randomRIO ('A', 'Z')
  let randDigit  = randomRIO ('0', '9')
  letters <- sequence [randLetter, randLetter]
  digits  <- sequence [randDigit, randDigit, randDigit]
  return (letters ++ digits)
