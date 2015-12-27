module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Car
import GameField

width , height , offset, fps :: Int
width = 800
height = 600
offset = 100
fps = 30


main :: IO()
main = play window background fps initField drawField handleKeys updateField


window :: Display
window = InWindow "Tron" (width, height) (offset, offset)

background :: Color
background = black
