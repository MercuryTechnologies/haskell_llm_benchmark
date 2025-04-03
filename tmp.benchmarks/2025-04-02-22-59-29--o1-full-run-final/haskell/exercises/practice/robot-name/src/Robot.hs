module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, gets, modify, liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (replicateM)

data Robot = Robot
  { robotRef :: IORef String
  }

data RunState = RunState
  { usedNames :: Set String
  }

initialState :: RunState
initialState = RunState Set.empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
  newName <- generateUniqueName
  ref <- liftIO $ newIORef newName
  return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  newName <- generateUniqueName
  liftIO $ writeIORef ref newName

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

-- Helper function to generate a globally unique name using the RunState.
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
  st <- gets usedNames
  candidate <- liftIO randomName
  if candidate `Set.member` st
    then generateUniqueName
    else do
      modify (\s -> s { usedNames = Set.insert candidate (usedNames s) })
      return candidate

-- Generates a random name of two uppercase letters followed by three digits.
randomName :: IO String
randomName = do
  letters <- replicateM 2 $ randomRIO ('A', 'Z')
  digits  <- replicateM 3 $ randomRIO ('0', '9')
  return (letters ++ digits)
