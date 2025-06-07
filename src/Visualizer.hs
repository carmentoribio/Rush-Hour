module Visualizer (runVisualizer) where

import BoardUtils (Board, Car (..))
import Data.Map qualified as Map
import Difficulty (classifyDifficulty)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Step = Int

data World = World
  { steps :: [Board], -- Lista de tableros que representan los pasos de la solución
    current :: Step, -- Índice del paso actual en la lista de pasos
    playing :: Bool, -- Indica si el visualizador está en modo "play" (reproducción automática)
    difficultyLabel :: String -- Etiqueta de dificultad del tablero
  }

tileSize :: Float
tileSize = 80

boardSize :: Int
boardSize = 6

windowWidth, windowHeight :: Int
windowWidth = boardSize * round tileSize + 250 -- espacio extra para mostrar el nivel de dificultad
windowHeight = boardSize * round tileSize + 250 -- espacio extra para mostrar el contador de pasos entre otros

-- ========================= ASIGNACIÓN DE COLORES =========================
-- Asigna un color a cada coche basado en su ID (letra mayúscula)
carColor :: Char -> Color
carColor 'o' = greyN 0.2
carColor 'A' = red
carColor char = makeColor r g b 1.0 -- Crea un color RGB a partir de HSV, con opacidad 1.0 (100% opaco)
  where
    idx = fromEnum char - fromEnum 'A'
    total = 26 -- Total de letras mayúsculas (A-Z)
    jump = 11 -- Coprimo de 26 para mezclar todos los colores
    scrambledIdx = (idx * jump) `mod` total -- Índice modificado para mezclar colores y que no se asignen en una tonalidad lineal (para aumentar la diversidad de colores y el contraste en general)
    -- Saltar el rango rojo (0-20° y 340-360°) para evitar coches con tonalidades rojas
    hueBase = fromIntegral scrambledIdx * (360 / fromIntegral total)
    hue
      | hueBase < 20 = hueBase + 30
      | hueBase > 340 = hueBase - 30
      | otherwise = hueBase
    (r, g, b) = hsvToRGB hue 1.0 1.0
    hsvToRGB :: Float -> Float -> Float -> (Float, Float, Float)
    hsvToRGB h s v = (r' + m, g' + m, b' + m)
      where
        c = v * s
        x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
        m = v - c
        (r', g', b') = case (floor (h / 60) :: Int) `mod` 6 of
          0 -> (c, x, 0)
          1 -> (x, c, 0)
          2 -> (0, c, x)
          3 -> (0, x, c)
          4 -> (x, 0, c)
          5 -> (c, 0, x)
          _ -> (0, 0, 0)

    -- Modulo para Float
    mod' :: Float -> Float -> Float
    mod' x y = x - fromIntegral (floor (x / y) :: Int) * y

-- ========================================================================

-- Dibuja el tablero actual
drawBoard :: Board -> Picture
drawBoard cars = pictures [drawTile (r, c) | r <- [0 .. 5], c <- [0 .. 5]]
  where
    posMap = Map.fromList [((r, c), carId car) | car <- cars, (r, c) <- positions car]
    drawTile (r, c) =
      translate (fromIntegral c * tileSize - 240) (240 - fromIntegral r * tileSize) $
        color col $
          rectangleSolid tileSize tileSize
      where
        ch = Map.findWithDefault 'o' (r, c) posMap
        col = carColor ch

-- Dibuja la ventana completa: el tablero, el nivel de dificultad, el contador de pasos, etc.
drawWorld :: World -> Picture
drawWorld w =
  pictures
    [ drawBoard (steps w !! current w), -- Tablero
      translate (-200) 300 $ scale 0.15 0.15 $ color white $ text $ "Difficulty: " ++ difficultyLabel w, -- Etiqueta que muestra la dificultad del nivel
      translate 150 (-270) $ scale 0.15 0.15 $ color white $ text $ "Step: " ++ show (current w) ++ "/" ++ show (length (steps w) - 1), -- Contador de pasos
      translate (-200) (-270) $ scale 0.15 0.15 $ color (if playing w then green else white) $ text "Press SPACE to Play/Pause", -- Instrucciones de Reproducir/Pausar
      translate (-200) (-300) $ scale 0.15 0.15 $ color white $ text "Press ENTER to Restart", -- Instrucciones para empezar el nivel de 0
      translate (tileSize * 3) tileSize $ scale 0.15 0.15 $ color white $ text "-> EXIT" -- Flecha que muestra la salida
    ]

-- Manejo de eventos
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = w {playing = not (playing w)} -- Pulsar ESPACIO alterna Play/Pause
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) w = w {current = 0, playing = False} -- Pulsar ENTER reinicia el nivel (y para la animación)
handleEvent _ w = w

-- Avanza automáticamente si está en modo "play"
updateWorld :: Float -> World -> World
updateWorld _ w
  | playing w && current w < length (steps w) - 1 = w {current = current w + 1}
  | otherwise = w

-- Función principal
runVisualizer :: [Board] -> IO ()
runVisualizer solution = do
  let diff = classifyDifficulty (head solution) solution
      initialWorld = World solution 0 False diff
  play
    (InWindow "Rush Hour Visualizer" (windowWidth, windowHeight) (100, 100))
    black -- color de fondo
    2 -- actualizaciones por segundo
    initialWorld -- estado inicial
    drawWorld -- función para dibujar
    handleEvent -- función para manejar eventos
    updateWorld -- función para actualizar el mundo
