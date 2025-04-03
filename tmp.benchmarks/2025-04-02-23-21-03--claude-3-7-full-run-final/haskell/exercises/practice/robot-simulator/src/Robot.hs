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

data Robot = Robot { robotBearing :: Bearing
                   , robotCoordinates :: (Integer, Integer)
                   } deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing = robotBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot [] = robot
move robot (instruction:rest) = move (executeInstruction robot instruction) rest
  where
    executeInstruction :: Robot -> Char -> Robot
    executeInstruction r 'R' = r { robotBearing = turnRight (robotBearing r) }
    executeInstruction r 'L' = r { robotBearing = turnLeft (robotBearing r) }
    executeInstruction r 'A' = r { robotCoordinates = advance (robotBearing r) (robotCoordinates r) }
    executeInstruction r _ = r  -- Ignore invalid instructions

    turnRight :: Bearing -> Bearing
    turnRight North = East
    turnRight East = South
    turnRight South = West
    turnRight West = North

    turnLeft :: Bearing -> Bearing
    turnLeft North = West
    turnLeft West = South
    turnLeft South = East
    turnLeft East = North

    advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
    advance North (x, y) = (x, y + 1)
    advance East (x, y) = (x + 1, y)
    advance South (x, y) = (x, y - 1)
    advance West (x, y) = (x - 1, y)
