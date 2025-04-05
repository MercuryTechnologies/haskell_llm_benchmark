module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Char (chr, ord)

data Robot = Robot (IORef String)
data RunState = RunState [String]

initialState :: RunState
initialState = RunState []

mkRobot :: StateT RunState IO Robot
mkRobot = do
    RunState usedNames <- get
    (name, newUsedNames) <- liftIO $ generateUniqueName usedNames
    put (RunState newUsedNames)
    ref <- liftIO $ newIORef name
    return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
    RunState usedNames <- get
    oldName <- liftIO $ readIORef ref
    let filteredNames = filter (/= oldName) usedNames
    (newName, newUsedNames) <- liftIO $ generateUniqueName filteredNames
    put (RunState newUsedNames)
    liftIO $ writeIORef ref newName

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

generateUniqueName :: [String] -> IO (String, [String])
generateUniqueName usedNames = do
    name <- generateRandomName
    if name `elem` usedNames
        then generateUniqueName usedNames
        else return (name, name:usedNames)

generateRandomName :: IO String
generateRandomName = do
    c1 <- randomRIO ('A', 'Z')
    c2 <- randomRIO ('A', 'Z')
    d1 <- randomRIO (0, 9)
    d2 <- randomRIO (0, 9)
    d3 <- randomRIO (0, 9)
    return [c1, c2, intToDigit d1, intToDigit d2, intToDigit d3]

intToDigit :: Int -> Char
intToDigit i = chr (ord '0' + i)
