module Robot
    ( Bearing(East,North,South,West)
    , getBearing
    , getCoordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }

getBearing :: Robot -> Bearing
getBearing robot = bearing robot

getCoordinates :: Robot -> (Integer, Integer)
getCoordinates robot = coordinates robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot direction coords

move :: Robot -> String -> Robot
move robot instructions = foldl moveRobot robot instructions
  where
    moveRobot r 'R' = r { bearing = turnRight (bearing r) }
    moveRobot r 'L' = r { bearing = turnLeft (bearing r) }
    moveRobot r 'A' = r { coordinates = advance (bearing r) (coordinates r) }
    moveRobot r _   = r

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance North (x, y) = (x, y + 1)
advance East  (x, y) = (x + 1, y)
advance South (x, y) = (x, y - 1)
advance West  (x, y) = (x - 1, y)
