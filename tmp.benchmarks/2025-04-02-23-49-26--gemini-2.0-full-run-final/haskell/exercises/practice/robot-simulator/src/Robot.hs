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

data Robot = Robot {
    robotBearing :: Bearing,
    robotCoordinates :: (Integer, Integer)
} deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing robot = robotBearing robot

coordinates :: Robot -> (Integer, Integer)
coordinates robot = robotCoordinates robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot { robotBearing = direction, robotCoordinates = coords }

move :: Robot -> String -> Robot
move robot instructions = foldl moveOne robot instructions
  where
    moveOne :: Robot -> Char -> Robot
    moveOne r 'R' = turnRight r
    moveOne r 'L' = turnLeft r
    moveOne r 'A' = advance r
    moveOne r _   = r  -- Ignore invalid instructions

    turnRight :: Robot -> Robot
    turnRight robot = robot { robotBearing = newBearing }
      where
        newBearing = case robotBearing robot of
            North -> East
            East  -> South
            South -> West
            West  -> North

    turnLeft :: Robot -> Robot
    turnLeft robot = robot { robotBearing = newBearing }
      where
        newBearing = case robotBearing robot of
            North -> West
            West  -> South
            South -> East
            East  -> North

    advance :: Robot -> Robot
    advance robot = robot { robotCoordinates = newCoordinates }
      where
        (x, y) = robotCoordinates robot
        newCoordinates = case robotBearing robot of
            North -> (x, y + 1)
            East  -> (x + 1, y)
            South -> (x, y - 1)
            West  -> (x - 1, y)
