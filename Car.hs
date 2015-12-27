module Car where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Line
import Data.Maybe

data Car = Car {
  start_position :: (Float, Float),
  position :: (Float, Float),
  start_dir :: (Float, Float),
  direcction :: (Float, Float),
  speed :: Float,
  carWidth :: Float,
  carHeight :: Float,
  carPath :: [(Float, Float)],
  carColor :: Color
} deriving(Show)

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

    x' = (fst $ position car) + (carWidth car / 2) * (fst $ direcction car) + (carHeight car / 2) * (snd $ direcction car)
    y' = (snd $ position car) - (carHeight car / 2) * (fst $ direcction car) + (snd $ direcction car) * (carWidth car / 2)

rightBottomPoint :: Car -> (Float, Float)
rightBottomPoint car = (x', y')
  where
    x' = (fst $ position car) + (carWidth car / 2) * (fst $ direcction car) - (carHeight car / 2) * (snd $ direcction car)
    y' = (snd $ position car) + (carHeight car / 2) *(fst $ direcction car) + (snd $ direcction car) * (carWidth car / 2)

leftBottomPoint :: Car -> (Float, Float)
leftBottomPoint car = (x', y')
  where
    x' = (fst $ position car) - (carWidth car / 2) * (fst $ direcction car) - (carHeight car / 2) * (snd $ direcction car)
    y' = (snd $ position car) + (carHeight car / 2) * (fst $ direcction car) - (snd $ direcction car) * (carWidth car / 2)

leftTopPoint :: Car -> (Float, Float)
leftTopPoint car = (x', y')
  where
    x' = (fst $ position car) - (carWidth car / 2) * (fst $ direcction car) + (carHeight car / 2) * (snd $ direcction car)
    y' = (snd $ position car) - (carHeight car / 2) * (fst $ direcction car) - (snd $ direcction car) * (carWidth car / 2)



moveCar :: Float -> [(Float,Float)] -> Car -> Car -> Car
moveCar seconds walls car enemy = if checkCol car (carPath car) || checkCol car (carPath enemy) || checkCol car walls then
  car {carPath = [(start_position car)], position = (start_position car), direcction = (start_dir car)}
  else car {carPath = updatePath (dx, dy) (carPath car), position = (x', y')}
  where
    (x,y) = position car
    (vx, vy) = direcction car
    v = speed car
    x' = x + vx * v * seconds
    y' = y + vy * v * seconds
    dx = x' - vx * carWidth car
    dy = y' - vy * carWidth car

updatePath :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
updatePath (x1, y1) [] = [(x1,y1)]
updatePath (x1, y1) [x] = (x1, y1) : [x]
updatePath (x1, y1) (x:y:xs)
  | fst x == fst y = if x1 == fst x then
        (x1,y1):(y:xs)
      else (x1, y1):(x:y:xs)
  | snd x == snd y = if y1 == snd x then
      (x1,y1):(y:xs)
    else (x1, y1):(x:y:xs)
  | otherwise = (x1, y1):(x:y:xs)

checkCol :: Car -> [(Float, Float)] -> Bool
checkCol car (x:xs) = checkCol' car x xs
  where
    checkCol' car x [] = False
    checkCol' car x (y:xs) = checkCarCollision car x y || checkCol' car y xs

checkCarCollision :: Car -> (Float, Float) -> (Float, Float) -> Bool
checkCarCollision car p1 p2 = checkCollision p1 p2 (rightTopPoint car) (rightBottomPoint car)  ||
   checkCollision p1 p2 (rightBottomPoint car) (leftBottomPoint car) ||
   --checkCollision p1 p2 (leftBottomPoint car) (leftTopPoint car) ||
   checkCollision p1 p2 (leftTopPoint car) (rightTopPoint car)

checkCollision :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
checkCollision lp1 lp2 cp1 cp2 = isJust p'
    where
      p' = intersectSegSeg lp1 lp2 cp1 cp2

-- initCar :: Car
-- initCar = Car { carWidth = 2,
--   carHeight = 2,
--   position = (0,0),
--   direcction = (0,1),
--   speed = 140,
--   carPath = [(0,0)]
-- }
