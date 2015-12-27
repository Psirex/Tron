module Car where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Line

data CarPhysic = CarPhysic {
    location :: Vector,
    carHeading :: Float,
    carSpeed :: Float,
    steerAngle :: Float,
    wheelBase :: Float

}

data Car = Car {
  position :: (Float, Float),
  direcction :: (Float, Float),
  speed :: Float,
  carWidth :: Float,
  carHeight :: Float,
  carPath :: [(Float, Float)]
}

drawCar :: Car -> Picture
drawCar car = pictures [car', path]
  where
    car' = uncurry translate (position car) $ rotate (angleBtw (direcction car) (1,0)) $ color blue $ rectangleWire (carWidth car) (carHeight car)
    path = color blue $ line $ carPath car

angleBtw :: (Float, Float) -> (Float, Float) -> Float
angleBtw (x1, y1) (x2, y2) = phi
  where
    v1 = (x1, y1)
    v2 = (x2, y2)
    phi = angleVV v1 v2 * 180 / 3.14


rightTopPoint :: Car -> (Float, Float)
rightTopPoint car = (x', y')
  where
    x' = (fst $ position car) + carWidth car / 2
    y' = (snd $ position car) - carHeight car / 2

rightBottomPoint :: Car -> (Float, Float)
rightBottomPoint car = (x', y')
  where
    x' = (fst $ position car) + carWidth car / 2
    y' = (snd $ position car) + carHeight car / 2

leftBottomPoint :: Car -> (Float, Float)
leftBottomPoint car = (x', y')
  where
    x' = (fst $ position car) - carWidth car / 2
    y' = (snd $ position car) + carHeight car / 2

leftTopPoint :: Car -> (Float, Float)
leftTopPoint car = (x', y')
  where
    x' = (fst $ position car) - carWidth car / 2
    y' = (snd $ position car) - carHeight car / 2

-- linesFromPoint :: [(Point, Point)] -> [Line]
-- linesFromPoint (x:xs) = linesFromPoint' x xs []

-- checkIntersect :: Line -> [(Point, Point)] -> Bool
-- checkIntersect car xs =

forwSide :: Car -> Line
forwSide car = fromPoint (rightTopPoint car) (rightBottomPoint car)

rightSide :: Car -> Line
rightSide car = fromPoint (rightBottomPoint car) (leftBottomPoint car)

botmSide :: Car -> Line
botmSide car = fromPoint (leftBottomPoint car) (leftTopPoint car)

leftSide :: Car -> Line
leftSide car = fromPoint (leftTopPoint car) (rightTopPoint car)

moveCar :: Float -> Car -> Car
moveCar seconds car = car {carPath = position car : carPath car, position = (x', y')}
  where
    (x,y) = position car
    (vx, vy) = direcction car
    v = speed car
    x' = x + vx * v * seconds
    y' = y + vy * v * seconds

-- checkCollision (Float, Float) -> (Float, Float) ->
--                 (Float, Float) -> (Float, Float) ->
--                 (Float, Float) -> (Float, Float) ->
--                 (Float, Float) -> (Float, Float) -> Bool
-- checkCollision = undefined
initCar :: Car
initCar = Car { carWidth = 25,
  carHeight = 13,
  position = (-40,40),
  direcction = (1,0),
  speed = 100,
  carPath = []
}
