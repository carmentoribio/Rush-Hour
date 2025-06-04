module Visualizer (runVisualizer) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import BoardUtils (Board, Car(..))
import qualified Data.Map as Map

type Step = Int
data World = World { steps :: [Board], current :: Step, playing :: Bool }

tileSize :: Float
tileSize = 80

boardSize :: Int
boardSize = 6

windowWidth, windowHeight :: Int
windowWidth = boardSize * round tileSize
windowHeight = boardSize * round tileSize + 50 -- espacio extra para contador

-- Asigna colores a identificadores de coche
carColor :: Char -> Color
carColor 'o' = greyN 0.2
carColor 'A' = red
carColor 'B' = blue
carColor 'C' = green
carColor 'D' = orange
carColor 'E' = violet
carColor _   = makeColorI 100 100 255 255

-- Dibuja el tablero actual
drawBoard :: Board -> Picture
drawBoard cars =
  let posMap = Map.fromList [((r, c), carId car) | car <- cars, (r, c) <- positions car]
      drawTile (r, c) =
        let ch = Map.findWithDefault 'o' (r, c) posMap
            col = carColor ch
         in translate (fromIntegral c * tileSize - 240) (240 - fromIntegral r * tileSize)
              $ color col
              $ rectangleSolid tileSize tileSize
   in pictures [drawTile (r, c) | r <- [0..5], c <- [0..5]]

-- Dibuja contador y botón "Play"
drawWorld :: World -> Picture
drawWorld w =
  pictures
    [ drawBoard (steps w !! current w)
    , translate 150 (-270) $ scale 0.15 0.15 $ color white $ text $ "Paso: " ++ show (current w) ++ "/" ++ show (length (steps w) - 1)
    , translate (-200) (-270) $ scale 0.15 0.15 $ color (if playing w then green else white) $ text "Presiona espacio para Play/Pausa"
    ]

-- Manejo de eventos
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = w { playing = not (playing w) }
handleEvent _ w = w

-- Avanza automáticamente si está en modo "play"
updateWorld :: Float -> World -> World
updateWorld dt w
  | playing w && current w < length (steps w) - 1 = w { current = current w + 1 }
  | otherwise = w

-- Función principal
runVisualizer :: [Board] -> IO ()
runVisualizer solution = do
  let initialWorld = World solution 0 False
  play
    (InWindow "Rush Hour Visualizer" (windowWidth, windowHeight) (100, 100))
    black         -- color de fondo
    2             -- actualizaciones por segundo
    initialWorld  -- estado inicial
    drawWorld     -- función para dibujar
    handleEvent   -- función para manejar eventos
    updateWorld   -- función para actualizar el mundo
