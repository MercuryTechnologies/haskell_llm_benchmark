module Robot
    ( Bearing(North,East,South,West)
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

data Robot = Robot { robotBearing :: Bearing, robotCoordinates :: (Integer, Integer) }
           deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot d _) = d

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ pos) = pos

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move = foldl processInstruction

processInstruction :: Robot -> Char -> Robot
processInstruction robot 'R' = robot { robotBearing = turnRight (bearing robot) }
processInstruction robot 'L' = robot { robotBearing = turnLeft (bearing robot) }
processInstruction (Robot d (x, y)) 'A' =
    case d of
      North -> Robot d (x, y + 1)
      East  -> Robot d (x + 1, y)
      South -> Robot d (x, y - 1)
      West  -> Robot d (x - 1, y)
processInstruction robot _ = robot

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
