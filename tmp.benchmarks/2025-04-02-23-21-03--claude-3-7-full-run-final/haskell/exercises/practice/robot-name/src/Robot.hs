module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put)
import System.Random (randomRIO)
import Data.Char (chr)
import Data.List (delete)
import Control.Monad (replicateM)

-- Robot has a name
data Robot = Robot { name :: String }

-- RunState keeps track of used names
data RunState = RunState { usedNames :: [String] }

initialState :: RunState
initialState = RunState { usedNames = [] }

-- Generate a random letter (A-Z)
randomLetter :: IO Char
randomLetter = do
  n <- randomRIO (0, 25)
  return $ chr (n + 65)  -- 'A' is ASCII 65

-- Generate a random digit (0-9)
randomDigit :: IO Char
randomDigit = do
  n <- randomRIO (0, 9)
  return $ chr (n + 48)  -- '0' is ASCII 48

-- Generate a random name (2 letters followed by 3 digits)
generateName :: IO String
generateName = do
  letters <- replicateM 2 randomLetter
  digits <- replicateM 3 randomDigit
  return $ letters ++ digits

-- Generate a unique name not in the used names list
uniqueName :: [String] -> IO String
uniqueName used = do
  name <- generateName
  if name `elem` used
    then uniqueName used  -- Try again if name is already used
    else return name

-- Create a new robot with a unique name
mkRobot :: StateT RunState IO Robot
mkRobot = do
  state <- get
  let used = usedNames state
  newName <- uniqueName used
  put $ RunState (newName : used)
  return $ Robot newName

-- Reset a robot's name to a new unique name
resetName :: Robot -> StateT RunState IO ()
resetName (Robot oldName) = do
  state <- get
  let used = usedNames state
  newName <- uniqueName used
  put $ RunState ((newName : delete oldName used))

-- Get a robot's name
robotName :: Robot -> IO String
robotName = return . name
