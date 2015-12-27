module Car where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Maybe
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

--
-- *********
-- *     ->* - расположение машины начальное
-- *********

--
forwSide :: Car -> Line
forwSide car = fromPoint (rightTopPoint car) (rightBottomPoint car)

rightSide :: Car -> Line
rightSide car = fromPoint (rightBottomPoint car) (leftBottomPoint car)

bottomSide :: Car -> Line
bottomSide car = fromPoint (leftBottomPoint car) (leftTopPoint car)

leftSide :: Car -> Line
leftSide car = fromPoint (leftTopPoint car) (rightTopPoint car)

moveCar :: Float -> Car -> Car
moveCar seconds car = if checkCol car (carPath car) then
  car {carPath = (updatePath (x', y') (carPath car)), position = (0, 0)}
  else car {carPath = (updatePath (x' - (carWidth car), y') (carPath car)), position = (x', y')}
  where
    (x,y) = position car
    (vx, vy) = direcction car
    v = speed car
    x' = x + vx * v * seconds
    y' = y + vy * v * seconds


updatePath :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
updatePath (x1, y1) [] = [(x1,y1)]
updatePath (x1, y1) [x] = (x1, y1) : [x]
updatePath (x1, y1) (x:xs) = ((x1,y1) : xs)

inRange :: (Float, Float) -> (Float, Float) -> (Float,Float) -> Bool
inRange (x1, y1) (x2, y2) (x, y) = x <= x2 && x >= x1 && y <= y2 && y >= y1

checkCol :: Car -> [(Float, Float)] -> Bool
checkCol car [] = False
checkCol car [x] = False
checkCol car (x:y:xs) = (checkCarCollision car x y) || (checkCol car xs)

checkCarCollision :: Car -> (Float, Float) -> (Float, Float) -> Bool
checkCarCollision car p1 p2 = checkCollision (forwSide car) l1 (rightTopPoint car) (rightBottomPoint car)
  --checkCollision (rightSide car) l1 (rightBottomPoint car) (leftBottomPoint car) ||
  --checkCollision (bottomSide car) l1 (leftBottomPoint car) (leftTopPoint car)
  --checkCollision (leftSide car) l1 (leftTopPoint car) (rightTopPoint car)
    where
      l1 = fromPoint p1 p2

checkCollision :: Line -> Line -> (Float, Float) -> (Float, Float) -> Bool
checkCollision l1 l2 p1 p2 = isJust p' &&
  (inRange p1 p2 (fromJust p'))
    where
      p' = intersect l1 l2




initCar :: Car
initCar = Car { carWidth = 25,
  carHeight = 13,
  position = (-40,40),
  direcction = (1,0),
  speed = 100,
  carPath = []
}
