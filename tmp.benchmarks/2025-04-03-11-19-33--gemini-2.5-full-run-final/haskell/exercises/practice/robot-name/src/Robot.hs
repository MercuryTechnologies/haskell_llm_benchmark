module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar, retry)
import Control.Monad.State (StateT, get, liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)
import Data.Char (chr)

-- RunState holds the set of all currently assigned names to ensure uniqueness.
-- The TVar allows safe concurrent access and modification.
newtype RunState = RunState (TVar (Set String))

-- Robot holds its own name (or Nothing if unnamed/reset) in a TVar,
-- and a reference to the shared set of names for uniqueness checks.
data Robot = Robot (TVar (Maybe String)) (TVar (Set String))

-- Initialize the state with an empty set of names within a new TVar.
-- Needs IO because newTVar is an IO action.
initialState :: IO RunState
initialState = RunState <$> atomically (newTVar Set.empty)

-- Generate a random name candidate: 2 uppercase letters + 3 digits.
generateName :: IO String
generateName = do
    c1 <- randomRIO ('A', 'Z')
    c2 <- randomRIO ('A', 'Z')
    n1 <- randomRIO (0, 9 :: Int)
    n2 <- randomRIO (0, 9 :: Int)
    n3 <- randomRIO (0, 9 :: Int)
    return [c1, c2, chr (n1 + ord '0'), chr (n2 + ord '0'), chr (n3 + ord '0')]
  where
    ord = fromEnum -- Helper to convert Int digit to Char

-- Create a new robot within the StateT context.
mkRobot :: StateT RunState IO Robot
mkRobot = do
    RunState nameSetTVar <- get -- Get the shared name set TVar from the state
    -- Create a TVar for the robot's own name, initially Nothing.
    -- Needs liftIO because atomically/newTVar are IO/STM.
    robotNameTVar <- liftIO $ atomically $ newTVar Nothing
    -- Return a new Robot containing its name TVar and the shared set TVar.
    return $ Robot robotNameTVar nameSetTVar

-- Reset the robot's name back to Nothing.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot robotNameTVar _) =
    -- Atomically clear the robot's specific name TVar.
    -- Needs liftIO because atomically/writeTVar are IO/STM.
    liftIO $ atomically $ writeTVar robotNameTVar Nothing

-- Get the robot's name. If it doesn't have one, generate a unique one.
robotName :: Robot -> IO String
robotName (Robot robotNameTVar nameSetTVar) = do
    maybeName <- atomically $ readTVar robotNameTVar
    case maybeName of
        Just name -> return name -- Name exists, return it.
        Nothing   -> generateUniqueName -- Name doesn't exist, generate one.
  where
    -- Helper function to generate and assign a unique name.
    generateUniqueName :: IO String
    generateUniqueName = do
        candidate <- generateName -- Generate a potential name.
        -- Attempt to atomically assign the name if it's unique.
        atomically $ do
            nameSet <- readTVar nameSetTVar -- Read the current set of used names.
            if Set.member candidate nameSet
                then retry -- Name collision, STM transaction aborts and retries generateUniqueName.
                else do
                    -- Name is unique, assign it to the robot and update the shared set.
                    writeTVar robotNameTVar (Just candidate)
                    writeTVar nameSetTVar (Set.insert candidate nameSet)
                    return candidate -- Return the newly assigned name.
