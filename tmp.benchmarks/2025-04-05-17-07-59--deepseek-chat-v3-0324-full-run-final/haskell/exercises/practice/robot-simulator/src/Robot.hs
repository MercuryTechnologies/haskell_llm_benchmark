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

data Robot = Robot 
    { robotBearing :: Bearing
    , robotCoordinates :: (Integer, Integer)
    } deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing = robotBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move = foldl executeInstruction
  where
    executeInstruction robot 'R' = robot { robotBearing = turnRight (robotBearing robot) }
    executeInstruction robot 'L' = robot { robotBearing = turnLeft (robotBearing robot) }
    executeInstruction robot 'A' = robot { robotCoordinates = advance (robotBearing robot) (robotCoordinates robot) }
    executeInstruction robot _   = robot  -- ignore invalid instructions

    turnRight North = East
    turnRight East  = South
    turnRight South = West
    turnRight West  = North

    turnLeft North = West
    turnLeft West  = South
    turnLeft South = East
    turnLeft East  = North

    advance North (x, y) = (x, y + 1)
    advance East  (x, y) = (x + 1, y)
    advance South (x, y) = (x, y - 1)
    advance West  (x, y) = (x - 1, y)
