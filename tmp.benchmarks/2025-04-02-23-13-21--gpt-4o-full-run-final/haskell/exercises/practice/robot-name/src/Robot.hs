module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.List (delete)

data Robot = Robot { name :: String }
data RunState = RunState { usedNames :: [String] }

initialState :: RunState
initialState = RunState { usedNames = [] }

mkRobot :: StateT RunState IO Robot
mkRobot = do
    state <- get
    newName <- liftIO $ generateUniqueName (usedNames state)
    let robot = Robot { name = newName }
    put state { usedNames = newName : usedNames state }
    return robot

resetName :: Robot -> StateT RunState IO ()
resetName robot = do
    state <- get
    let currentName = name robot
    newName <- liftIO $ generateUniqueName (usedNames state)
    put state { usedNames = newName : delete currentName (usedNames state) }
    return ()

robotName :: Robot -> IO String
robotName robot = return (name robot)

generateUniqueName :: [String] -> IO String
generateUniqueName used = do
    newName <- generateName
    if newName `elem` used
        then generateUniqueName used
        else return newName

generateName :: IO String
generateName = do
    letters <- sequence [randomRIO ('A', 'Z'), randomRIO ('A', 'Z')]
    digits <- sequence [randomRIO ('0', '9'), randomRIO ('0', '9'), randomRIO ('0', '9')]
    return (letters ++ digits)
