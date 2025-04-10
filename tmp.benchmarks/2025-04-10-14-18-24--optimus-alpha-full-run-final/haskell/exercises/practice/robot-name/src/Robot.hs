module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, liftIO)
import Data.IORef
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

-- Robot is a mutable reference to its name (which may be Nothing if not yet assigned)
newtype Robot = Robot (IORef (Maybe String))

-- RunState tracks all assigned names to ensure uniqueness
newtype RunState = RunState (IORef (Set String))

initialState :: RunState
initialState = RunState $ unsafePerformIO $ newIORef Set.empty
{-# NOINLINE initialState #-}

-- Helper to generate a random robot name in the format "AA000"
randomName :: IO String
randomName = do
    l1 <- randomRIO ('A', 'Z')
    l2 <- randomRIO ('A', 'Z')
    d1 <- randomRIO ('0', '9')
    d2 <- randomRIO ('0', '9')
    d3 <- randomRIO ('0', '9')
    return [l1, l2, d1, d2, d3]

-- Helper to generate a unique name, given the set of used names
generateUniqueName :: IORef (Set String) -> IO String
generateUniqueName usedRef = do
    let try = do
            name <- randomName
            used <- readIORef usedRef
            if Set.member name used
                then try
                else do
                    writeIORef usedRef (Set.insert name used)
                    return name
    try

mkRobot :: StateT RunState IO Robot
mkRobot = do
    RunState usedRef <- get
    name <- liftIO $ generateUniqueName usedRef
    nameRef <- liftIO $ newIORef (Just name)
    return (Robot nameRef)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
    mOldName <- liftIO $ readIORef nameRef
    RunState usedRef <- get
    case mOldName of
        Just oldName -> liftIO $ modifyIORef usedRef (Set.delete oldName)
        Nothing -> return ()
    liftIO $ writeIORef nameRef Nothing

robotName :: Robot -> IO String
robotName (Robot nameRef) = do
    mName <- readIORef nameRef
    case mName of
        Just name -> return name
        Nothing -> do
            -- Fallback: use a global set for uniqueness if called outside StateT context
            name <- generateUniqueName globalUsedNames
            writeIORef nameRef (Just name)
            return name

-- Global used names for robotName (since we can't access RunState in IO)
globalUsedNames :: IORef (Set String)
globalUsedNames = unsafePerformIO $ newIORef Set.empty
{-# NOINLINE globalUsedNames #-}
