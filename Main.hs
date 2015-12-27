module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.IO.Class
import Car
import GameField

width , height , offset, fps :: Int
width = 800
height = 600
offset = 100
fps = 60

main :: IO()
main = play window background fps initField drawField handleKeys updateField
  where
   
window :: Display
window = InWindow "Tron" (width, height) (offset, offset)

background :: Color
background = black
