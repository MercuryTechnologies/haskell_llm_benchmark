module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

import Data.Foldable (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

-- Define Robot using record syntax to store bearing and coordinates.
-- This automatically provides the 'bearing' and 'coordinates' accessor functions.
data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }
             deriving (Eq, Show)

-- mkRobot constructs a Robot value.
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot -- Using the Robot constructor directly

-- Helper function to turn the robot right.
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- Helper function to turn the robot left.
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- Helper function to advance the robot one step in its current direction.
advance :: Robot -> Robot
advance robot@(Robot { bearing = b, coordinates = (x, y) }) =
    case b of
        North -> robot { coordinates = (x, y + 1) }
        East  -> robot { coordinates = (x + 1, y) }
        South -> robot { coordinates = (x, y - 1) }
        West  -> robot { coordinates = (x - 1, y) }

-- Processes a single instruction character.
processInstruction :: Robot -> Char -> Robot
processInstruction robot 'L' = robot { bearing = turnLeft (bearing robot) }
processInstruction robot 'R' = robot { bearing = turnRight (bearing robot) }
processInstruction robot 'A' = advance robot
processInstruction robot _   = robot -- Ignore invalid instructions

-- move processes the instruction string using foldl'.
move :: Robot -> String -> Robot
move robot instructions = foldl' processInstruction robot instructions
