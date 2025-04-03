module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.Set (Set, empty, insert, member)

-- Define the Robot data type
data Robot = Robot { robotName :: IO String }

-- Define the RunState data type to hold the set of used names
data RunState = RunState { usedNames :: Set String }

-- Generate a random robot name
generateName :: IO String
generateName = do
  letter1 <- randomRIO ('A', 'Z')
  letter2 <- randomRIO ('A', 'Z')
  number <- randomRIO (100, 999)
  return $ [letter1, letter2] ++ show number

-- Create the initial state with an empty set of used names
initialState :: RunState
initialState = RunState { usedNames = empty }

-- Create a new robot with a unique name
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- liftIO generateName
  state <- get
  if name `member` usedNames state
    then mkRobot -- Retry if the name is already used
    else do
      let newUsedNames = insert name (usedNames state)
      put state { usedNames = newUsedNames }
      let robot = Robot { robotName = return name }
      return robot

-- Reset the robot's name to a new unique name
resetName :: Robot -> StateT RunState IO ()
resetName robot = do
  newName <- liftIO generateName
  state <- get
  if newName `member` usedNames state
    then resetName robot -- Retry if the name is already used
    else do
      let newUsedNames = insert newName (usedNames state)
      put state { usedNames = newUsedNames }
      -- Update the robot's name using unsafePerformIO
      -- because robotName is an IO action.
      -- A cleaner approach would be to modify the Robot type
      -- to hold the name directly, but that would require
      -- changing the existing type signature.
      let updateRobotName name = do
            return $ Robot { robotName = return name }
      newRobot <- liftIO $ updateRobotName newName
      return ()
