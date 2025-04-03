module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, modify)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (StdGen, mkStdGen, randomR)

-- | The Robot type holds a mutable reference to its current name.
newtype Robot = Robot (IORef String)

-- | The RunState holds the global state: the set of used names and the random generator.
data RunState = RunState {
    usedNames :: Set String,
    nameGenerator :: StdGen
}

-- | Initial state: empty name set and a default random generator.
-- The seed (42) is arbitrary; tests might use a different setup.
initialState :: RunState
initialState = RunState Set.empty (mkStdGen 42)

-- | Generates a random name in the format LLNNN (e.g., AB123).
mkName :: StdGen -> (String, StdGen)
mkName gen =
    let (c1, gen1) = randomR ('A', 'Z') gen
        (c2, gen2) = randomR ('A', 'Z') gen1
        (n1, gen3) = randomR ('0', '9') gen2
        (n2, gen4) = randomR ('0', '9') gen3
        (n3, gen5) = randomR ('0', '9') gen4
    in ([c1, c2, n1, n2, n3], gen5)

-- | Generates a unique name by repeatedly calling mkName until an unused name is found.
-- Updates the RunState with the new name added to the set and the updated generator.
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
    state <- get
    let gen = nameGenerator state
    let names = usedNames state
    let (newName, newGen) = mkName gen
    if Set.member newName names
        then do -- Collision, update generator and try again
            put state { nameGenerator = newGen }
            generateUniqueName
        else do -- Unique name found, update state and return
            put state { usedNames = Set.insert newName names, nameGenerator = newGen }
            return newName

-- | Creates a new robot with a unique random name.
mkRobot :: StateT RunState IO Robot
mkRobot = do
    name <- generateUniqueName
    nameRef <- liftIO $ newIORef name
    return (Robot nameRef)

-- | Resets the robot's name to a new unique random name.
-- The old name is removed from the set of used names.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
    oldName <- liftIO $ readIORef nameRef
    -- Remove old name from the used set
    modify (\s -> s { usedNames = Set.delete oldName (usedNames s) })
    -- Generate a new unique name
    newName <- generateUniqueName
    -- Update the robot's name reference
    liftIO $ writeIORef nameRef newName

-- | Returns the current name of the robot.
robotName :: Robot -> IO String
robotName (Robot nameRef) = readIORef nameRef
