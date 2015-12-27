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
    car2 :: Car
}deriving (Show)

newCar1 :: Car -> GameField -> GameField
newCar1 car1 (GameField _ car2) = GameField car1 car2

newCar2 :: Car -> GameField -> GameField
newCar2 car2 (GameField car1 _) = GameField car1 car2

drawField :: GameField -> Picture
drawField (GameField car1 car2) = pictures [car1', path1, car2', path2]
  where
    car1' = uncurry translate (position car1) $ rotate (angleBtw (direcction car1) (1,0)) $ color white $ rectangleWire (carWidth car1) (carHeight car1)
    path1 = color (carColor car1) $ line $ carPath car1
    car2' = uncurry translate (position car2) $ rotate (angleBtw (direcction car2) (1,0)) $ color white $ rectangleWire (carWidth car2) (carHeight car2)
    path2 = color (carColor car2) $ line $ carPath car2

updateField :: Float -> GameField -> GameField
updateField seconds (GameField car1 car2) = GameField (moveCar seconds car1 car2) (moveCar seconds car2 car1)

handleKeys :: Event -> GameField -> GameField
handleKeys (EventKey (Char 'w') _ _ _) (GameField car1 car2) =
  if direcction car1 /= (0 , -1) then
  	newCar1 (car1 {direcction = (0, 1)}) (GameField car1 car2)
  else (GameField car1 car2)
handleKeys (EventKey (Char 's') _ _ _) (GameField car1 car2) =
  if direcction car1 /= (0 , 1) then
  	newCar1 (car1 {direcction = (0, -1)}) (GameField car1 car2)
  else (GameField car1 car2)
handleKeys (EventKey (Char 'd') _ _ _) (GameField car1 car2) =
  if direcction car1 /= (-1 , 0) then
  	newCar1 (car1 {direcction = (1, 0)}) (GameField car1 car2)
  else (GameField car1 car2)
handleKeys (EventKey (Char 'a') _ _ _) (GameField car1 car2) =
  if direcction car1 /= (1 , 0) then
  	newCar1 (car1 {direcction = (-1, 0)}) (GameField car1 car2)
  else (GameField car1 car2)

handleKeys (EventKey (SpecialKey KeyUp) _ _ _) (GameField car1 car2) =
  if direcction car2 /= (0 , -1) then
  	newCar2 (car2 {direcction = (0, 1)}) (GameField car1 car2)
  else (GameField car1 car2)
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) (GameField car1 car2) =
  if direcction car2 /= (0 , 1) then
  	newCar2 (car2 {direcction = (0, -1)}) (GameField car1 car2)
  else (GameField car1 car2)
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) (GameField car1 car2) =
  if direcction car2 /= (-1 , 0) then
  	newCar2 (car2 {direcction = (1, 0)}) (GameField car1 car2)
  else (GameField car1 car2)
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) (GameField car1 car2) =
  if direcction car2 /= (1 , 0) then
  	newCar2 (car2 {direcction = (-1, 0)}) (GameField car1 car2)
  else (GameField car1 car2)
 --рестарт
handleKeys (EventKey (Char 'r') _ _ _) _ = initField

handleKeys _ gf = gf



initField :: GameField
initField = GameField (Car { carWidth = 1,
						  carHeight = 1,
						  position = (40,-250),
						  direcction = (0,1),
						  speed = 50,
						  carPath = [(40,-250)],
						  carColor = blue
						})
						(Car { carWidth = 1,
						  carHeight = 1,
						  position = (40,250),
						  direcction = (0,-1),
						  speed = 50,
						  carPath = [(40,250)],
						  carColor = orange
						})
