module GameField where
import Car

data GameField = GameField {
    car1 :: Car,
    car2 :: Car
}


drawField :: GameField -> Picture
drawField = undefined

updateField :: Float -> GameField -> GameField
updateField = undefined

handleKeys :: Event -> Car -> Car
handleKeys = undefined

initField :: GameField
initField = undefined
