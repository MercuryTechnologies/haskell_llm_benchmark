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
bearing (Robot dir _) = dir

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coords) = coords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot dir coords = Robot dir coords

move :: Robot -> String -> Robot
move robot [] = robot
move robot (x:xs) = move (step robot x) xs
  where
    step :: Robot -> Char -> Robot
    step (Robot dir (xPos, yPos)) instruction =
      case instruction of
        'R' -> Robot (turnRight dir) (xPos, yPos)
        'L' -> Robot (turnLeft dir) (xPos, yPos)
        'A' -> case dir of
                 North -> Robot dir (xPos, yPos + 1)
                 East  -> Robot dir (xPos + 1, yPos)
                 South -> Robot dir (xPos, yPos - 1)
                 West  -> Robot dir (xPos - 1, yPos)
        _   -> Robot dir (xPos, yPos) -- ignore invalid instructions

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North
