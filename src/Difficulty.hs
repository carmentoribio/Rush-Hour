{- TODO:
Ajustar los parámetros para clasificar correctamente los mapas
-}
module Difficulty where

import BoardUtils (Board, Car (..))
import Data.Map qualified as Map
import Debug.Trace (traceShow)

-- Estructura para métricas de solución
data SolutionMetrics = SolutionMetrics
  { steps :: Int, -- Número de movimientos en solución óptima
    movedCars :: Int, -- Cantidad de coches diferentes movidos
    carCount :: Int, -- Cantidad total de coches en el tablero
    symmetryScore :: Int -- Grado de simetría (0-100)
  }
  deriving (Show)

classifyDifficulty ::
  Board -> -- El tablero inicial
  [Board] -> -- Lista de tableros que representan la solución
  String
-- PRE: El tablero debe ser una cadena de 36 caracteres (6x6) con mayúsculas (A-Z) y 'o' (espacio vacío).
-- POST: Devuelve una cadena que indica la categoría de dificultad del tablero ("Principiante", "Intermedio", "Avanzado" o "Experto").
classifyDifficulty board solutionPath =
  let metrics =
        SolutionMetrics
          { steps = length solutionPath - 1, -- Restamos 1 porque el primer estado es el inicial
            movedCars = countMovedCarsAllSteps solutionPath,
            carCount = length board,
            symmetryScore = calculateSymmetry board
          }
      score = difficultyScore metrics
   in traceShow (metrics, score) $ assignCategory score
  where
    difficultyScore :: SolutionMetrics -> Double
    -- PRE: Las métricas deben contener el número de pasos, coches movidos, coches iniciales y puntuación de simetría.
    -- POST: Devuelve un valor de dificultad entre 0 y 100.
    difficultyScore metrics =
      let stepScore = fromIntegral (steps metrics)
          movedCarsScore = fromIntegral (movedCars metrics) * (100 / fromIntegral (carCount metrics))
          simmetryScore = fromIntegral (100 - symmetryScore metrics)
       in 0.7 * stepScore
            + 0.2 * movedCarsScore
            + 0.1 * simmetryScore

    assignCategory :: Double -> String
    -- PRE: El score debe ser un valor entre 0 y 100.
    -- POST: Devuelve una cadena que indica la categoría de dificultad del tablero.
    assignCategory score
      | score < 35 = "BEGINNER"
      | score < 40 = "INTERMEDIATE"
      | score < 45 = "ADVANCED"
      | otherwise = "EXPERT"

countMovedCars ::
  Board -> -- Tablero inicial
  Board -> -- Tablero final
  Int -- Numero de coches que han cambiado de posición
  -- PRE: El tablero inicial y final deben ser listas de coches.
  -- POST: Devuelve el número de coches que han cambiado de posición entre el tablero inicial y final.
countMovedCars initial final =
  length [c | c <- final, carPositionsChanged c initial]
  where
    carPositionsChanged car = any (\c -> carId c == carId car && positions c /= positions car) -- compara las posiciones del coche en el tablero final con el inicial

countMovedCarsAllSteps :: [Board] -> Int
countMovedCarsAllSteps boards =
  length . Map.keys . Map.filter id $ foldl updateMoved Map.empty (zip boards (tail boards))
  where
    updateMoved acc (b1, b2) =
      foldl (\m car -> Map.insertWith (||) (carId car) (carPositionsChanged car b1) m) acc b2
    carPositionsChanged car = any (\c -> carId c == carId car && positions c /= positions car)

calculateSymmetry :: Board -> Int
-- PRE: El tablero debe ser una lista de coches.
-- POST: Devuelve un valor de simetría entre 0 y 100, donde 100 indica máxima simetría.
calculateSymmetry cars =
  max horizontalSymmetry verticalSymmetry -- Devolvemos el máximo entre la simetría horizontal y vertical
  where
    horizontalSymmetry = symmetryCheck (\(r, c) -> (r, 5 - c))
    verticalSymmetry = symmetryCheck (\(r, c) -> (5 - r, c))

    symmetryCheck transform =
      let mirrored = Map.fromList [(transform pos, car) | car <- cars, pos <- positions car] -- Creamos un mapa de posiciones reflejadas
          matches = [car | car <- cars, all (`Map.member` mirrored) (positions car)] -- Comprobamos cuántos coches tienen posiciones que coinciden con sus reflejos
       in (length matches * 100) `div` length cars -- FIXME: no sé si esto da un númeero normal