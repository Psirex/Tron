module GameField where
import Car
--import qualified Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
--import Control.Monad.IO.Class
--import System.IO.Unsafe

data GameField = GameField {
    car1 :: Car,
    car2 :: Car,
    walls :: [(Float, Float)]
}deriving (Show)

newCar1 :: Car -> GameField -> GameField
newCar1 car1 (GameField _ car2 w) = GameField car1 car2 w

newCar2 :: Car -> GameField -> GameField
newCar2 car2 (GameField car1 _ w) = GameField car1 car2 w

drawField :: GameField -> Picture
drawField (GameField car1 car2 walls) = pictures [car1', path1, car2', path2, walls']
  where
    car1' = uncurry translate (position car1) $ rotate (angleBtw (direcction car1) (1,0)) $ color white $ rectangleWire (carWidth car1) (carHeight car1)
    path1 = color (carColor car1) $ line $ carPath car1
    car2' = uncurry translate (position car2) $ rotate (angleBtw (direcction car2) (1,0)) $ color white $ rectangleWire (carWidth car2) (carHeight car2)
    path2 = color (carColor car2) $ line $ carPath car2
    walls' = color green $ line $ walls

updateField :: Float -> GameField -> GameField
updateField seconds (GameField car1 car2 w) = GameField (moveCar seconds w car1 car2) (moveCar seconds w car2 car1) w




handleKeys :: Event -> GameField -> GameField
handleKeys (EventKey (Char 'w') _ _ _) (GameField car1 car2 w) =
  if direcction car1 /= (0 , -1) then
  	newCar1 (car1 {direcction = (0, 1)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
handleKeys (EventKey (Char 's') _ _ _) (GameField car1 car2 w) =
  if direcction car1 /= (0 , 1) then
  	newCar1 (car1 {direcction = (0, -1)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
handleKeys (EventKey (Char 'd') _ _ _) (GameField car1 car2 w) =
  if direcction car1 /= (-1 , 0) then
  	newCar1 (car1 {direcction = (1, 0)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
handleKeys (EventKey (Char 'a') _ _ _) (GameField car1 car2 w) =
  if direcction car1 /= (1 , 0) then
  	newCar1 (car1 {direcction = (-1, 0)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) (GameField car1 car2 w) =
  if direcction car2 /= (0 , -1) then
  	newCar2 (car2 {direcction = (0, 1)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) (GameField car1 car2 w) =
  if direcction car2 /= (0 , 1) then
  	newCar2 (car2 {direcction = (0, -1)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) (GameField car1 car2 w) =
  if direcction car2 /= (-1 , 0) then
  	newCar2 (car2 {direcction = (1, 0)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) (GameField car1 car2 w) =
  if direcction car2 /= (1 , 0) then
  	newCar2 (car2 {direcction = (-1, 0)}) (GameField car1 car2 w)
  else (GameField car1 car2 w)
 --рестарт
handleKeys (EventKey (Char 'r') _ _ _) _ = initField

handleKeys _ gf = gf

size1 = 270

initField :: GameField
initField = GameField (Car { carWidth = 1,
						  carHeight = 1,
              start_position = (40,-250),
						  position = (40,-250),
              start_dir = (0, 1),
						  direcction = (0,1),
						  speed = 150,
						  carPath = [(40,-250)],
						  carColor = blue
						})
						(Car { carWidth = 1,
						  carHeight = 1,
              start_position = (40,250),
						  position = (40,250),
              start_dir = (0, -1),
						  direcction = (0,-1),
						  speed = 150,
						  carPath = [(40,250)],
						  carColor = orange
						})
            [(-size1, size1), (size1, size1), (size1, -size1), (-size1, -size1), (-size1, size1)]
