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

data Robot = Robot Bearing (Integer, Integer)
  deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coords) = coords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot dir coords = Robot dir coords

move :: Robot -> String -> Robot
move = foldl step
  where
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

    step (Robot dir coords) 'R' = Robot (turnRight dir) coords
    step (Robot dir coords) 'L' = Robot (turnLeft dir) coords
    step (Robot dir coords) 'A' = Robot dir (advance dir coords)
    step robot           _       = robot
