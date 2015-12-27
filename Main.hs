module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Car
width , height , offset, fps :: Int
width = 800
height = 600
offset = 100
fps = 60

data PongGame = Game {
  ballLoc :: (Float, Float),
  ballVel :: (Float, Float),
  player1 :: Float,
  player2 :: Float
} deriving(Show)


moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game {ballLoc = (x', y')}
  where
    (x,y) = ballLoc game
    (vx, vy) = ballVel game

    x' = x + vx * seconds
    y' = y + vy * seconds




initialState :: PongGame
initialState = Game { ballLoc = (-10, 30),
  ballVel = (10, -13),
  player1 = 40,
  player2 = -80
}

render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)


main :: IO()
main = play window background fps initCar drawCar handleKeys update
  where
    update :: Float -> Car -> Car
    update = moveCar

-- update :: ViewPort -> Float -> PongGame -> PongGame
-- update _ = moveBall

handleKeys :: Event -> Car -> Car
handleKeys (EventKey (Char 'w') _ _ _) car =
  if direcction car /= (0 , -1) then
    car {carPath = (position car) : (carPath car) , direcction = (0, 1)}
  else car
handleKeys (EventKey (Char 's') _ _ _) car =
  if direcction car /= (0 , 1) then
    car {carPath = (position car) : (carPath car), direcction = (0, -1)}
  else car
handleKeys (EventKey (Char 'd') _ _ _) car =
  if direcction car /= (-1 , 0) then
    car {carPath = (position car) : (carPath car), direcction = (1, 0)}
  else car
handleKeys (EventKey (Char 'a') _ _ _) car =
  if direcction car /= (1 , 0) then
    car {carPath = (position car) : (carPath car), direcction = (-1, 0)}
  else car
handleKeys _ car = car

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [translate (-20) (-100) $ color ballColor $ circleSolid 30,
                    translate 30 50 $ color paddleColor $ rectangleSolid 10 50]
          where
            ballColor = dark red
            paddleColor = light $ light blue