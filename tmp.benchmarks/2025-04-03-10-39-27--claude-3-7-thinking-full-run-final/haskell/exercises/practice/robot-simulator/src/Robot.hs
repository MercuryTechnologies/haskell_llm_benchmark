module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

-- Define the Robot with its bearing and coordinates
data Robot = Robot 
    { robotBearing :: Bearing
    , robotCoordinates :: (Integer, Integer)
    } deriving (Eq, Show)

-- Get the current bearing
bearing :: Robot -> Bearing
bearing = robotBearing

-- Get the current coordinates
coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

-- Create a new robot with initial bearing and coordinates
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- Helper function to turn right (clockwise)
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

-- Helper function to turn left (counter-clockwise)
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

-- Helper function to advance in the current direction
advance :: Robot -> Robot
advance robot@(Robot b (x, y)) = case b of
    North -> robot { robotCoordinates = (x, y + 1) }
    East  -> robot { robotCoordinates = (x + 1, y) }
    South -> robot { robotCoordinates = (x, y - 1) }
    West  -> robot { robotCoordinates = (x - 1, y) }

-- Execute a single instruction
executeInstruction :: Robot -> Char -> Robot
executeInstruction robot 'R' = robot { robotBearing = turnRight (bearing robot) }
executeInstruction robot 'L' = robot { robotBearing = turnLeft (bearing robot) }
executeInstruction robot 'A' = advance robot
executeInstruction robot _ = robot  -- Ignore any other instructions

-- Move the robot according to a string of instructions
move :: Robot -> String -> Robot
move = foldl executeInstruction
