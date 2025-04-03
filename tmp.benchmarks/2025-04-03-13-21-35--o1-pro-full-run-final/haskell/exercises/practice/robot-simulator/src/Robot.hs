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
coordinates (Robot _ c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b coord = Robot b coord

move :: Robot -> String -> Robot
move = foldl applyInstruction
  where
    applyInstruction :: Robot -> Char -> Robot
    applyInstruction (Robot b c) 'R' = Robot (turnRight b) c
    applyInstruction (Robot b c) 'L' = Robot (turnLeft b) c
    applyInstruction r@(Robot _ _) 'A' = advance r
    applyInstruction r _ = r

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

advance :: Robot -> Robot
advance (Robot North (x,y)) = Robot North (x, y+1)
advance (Robot East  (x,y)) = Robot East  (x+1, y)
advance (Robot South (x,y)) = Robot South (x, y-1)
advance (Robot West  (x,y)) = Robot West  (x-1, y)
