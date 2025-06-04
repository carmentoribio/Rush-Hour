module Difficulty where
import AStar(Car, Position, Orientation(..), carId, positions)
import Parse (parseMap, solution)
import Data.Map qualified as Map
import Data.List (find)

-- Estructura para métricas de solución
data SolutionMetrics = SolutionMetrics
  { steps :: Int          -- Número de movimientos en solución óptima
  , movedCars :: Int      -- Cantidad de coches diferentes movidos
  , carNumber :: Int      -- Cantidad total de coches en el tablero
  , symmetryScore :: Int  -- Grado de simetría (0-100)
  } deriving (Show)

classifyDifficulty :: String -> String
-- PRE: El tablero debe ser una cadena de 36 caracteres (6x6) con mayúsculas (A-Z) y 'o' (espacio vacío).
-- POST: Devuelve una cadena que indica la categoría de dificultad del tablero ("Principiante", "Intermedio", "Avanzado" o "Experto").
classifyDifficulty boardStr = case solution cars of -- Intenta encontrar una solución al tablero
    Nothing -> error "No tiene solución"
    Just solution ->
      let metrics = calculateMetrics cars solution
          score = difficultyScore metrics
      in assignCategory score
  where
    cars = parseMap boardStr

    -- FIXME: está fórmula es invent del Perplejo, como nos dice que un polinomio,
    -- cuando consigamos que funcione ajustamos para que se acerque a lo que queremos
    difficultyScore :: SolutionMetrics -> Double
    -- PRE: Las métricas deben contener el número de pasos, coches movidos, coches iniciales y puntuación de simetría.
    -- POST: Devuelve un valor de dificultad entre 0 y 100.
    difficultyScore metrics =
      0.5 * fromIntegral (steps metrics) +
      0.3 * fromIntegral (movedCars metrics) * (100 / fromIntegral (carNumber metrics)) +
      0.2 * fromIntegral (100 - symmetryScore metrics)

    --FIXME: otro invent del Perplejo, si quieres cambiarlo, adelante
    assignCategory :: Double -> String
    -- PRE: El score debe ser un valor entre 0 y 100.
    -- POST: Devuelve una cadena que indica la categoría de dificultad del tablero.
    assignCategory score
      | score < 30  = "Principiante"
      | score < 60  = "Intermedio"
      | score < 90  = "Avanzado"
      | otherwise   = "Experto"

calculateMetrics :: [Car] -> [[Car]] -> SolutionMetrics
-- PRE: El tablero inicial debe ser una lista de coches y la solución debe ser una lista de caminos.
-- POST: Devuelve un objeto SolutionMetrics con el número de pasos, coches movidos, coches iniciales y puntuación de simetría.
calculateMetrics initial solutionPath =
  SolutionMetrics
  { steps = length solutionPath - 1                        -- Restamos 1 porque el primer estado es el inicial
  , movedCars = countMovedCars initial (last solutionPath) -- Cuenta los coches que han cambiado de posición entre el inicial y el final
  , carNumber = length initial                             -- Cantidad total de coches en el tablero inicial
  , symmetryScore = calculateSymmetry initial              -- Calcula la simetría del tablero inicial
  }

countMovedCars :: [Car] -> [Car] -> Int
-- PRE: El tablero inicial y final deben ser listas de coches.
-- POST: Devuelve el número de coches que han cambiado de posición entre el tablero inicial y final.
countMovedCars initial final =
  length [c | c <- final, carPositionsChanged c initial]
  where
    carPositionsChanged car = any (\c -> carId c == carId car && positions c /= positions car) -- compara las posiciones del coche en el tablero final con el inicial

calculateSymmetry :: [Car] -> Int
-- PRE: El tablero debe ser una lista de coches.
-- POST: Devuelve un valor de simetría entre 0 y 100, donde 100 indica máxima simetría.
calculateSymmetry cars =
  max horizontalSymmetry verticalSymmetry -- Devolvemos el máximo entre la simetría horizontal y vertical
  where
    horizontalSymmetry = symmetryCheck (\(r,c) -> (r, 5-c))
    verticalSymmetry = symmetryCheck (\(r,c) -> (5-r, c))

    symmetryCheck transform =
      let mirrored = Map.fromList [(transform pos, car) | car <- cars, pos <- positions car] -- Creamos un mapa de posiciones reflejadas
          matches = [car | car <- cars, all (`Map.member` mirrored) (positions car)] -- Comprobamos cuántos coches tienen posiciones que coinciden con sus reflejos
      in (length matches * 100) `div` length cars --FIXME: no sé si esto da un númeero normal