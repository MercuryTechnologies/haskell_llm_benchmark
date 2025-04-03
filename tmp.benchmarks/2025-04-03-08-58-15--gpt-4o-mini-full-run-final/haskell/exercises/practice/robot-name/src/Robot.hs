module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put)
import System.Random (randomRIO)
import Data.Set (Set, empty, insert, member)

data Robot = Robot { name :: String } deriving (Show, Eq)
data RunState = RunState { existingNames :: Set String }

initialState :: RunState
initialState = RunState empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
    state <- get
    newName <- generateUniqueName (existingNames state)
    let newRobot = Robot newName
    put (state { existingNames = insert newName (existingNames state) })
    return newRobot

resetName :: Robot -> StateT RunState IO ()
resetName robot = do
    state <- get
    let updatedState = state { existingNames = insert (name robot) (existingNames state) }
    put updatedState

robotName :: Robot -> IO String
robotName robot = return (name robot)

generateUniqueName :: Set String -> IO String
generateUniqueName existing = do
    name <- randomName
    if member name existing
        then generateUniqueName existing
        else return name

randomName :: IO String
randomName = do
    letters <- sequence $ replicate 2 (randomRIO ('A', 'Z'))
    digits <- sequence $ replicate 3 (randomRIO ('0', '9'))
    return (letters ++ digits)
