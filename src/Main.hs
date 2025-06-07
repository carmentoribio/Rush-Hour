module Main where

import AStar (aStar)
import BoardUtils (Board, Car(..), Orientation(..), parseMap, getLegalMoves)
import Visualizer (runVisualizer)
import Data.List (find)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Para el cálculo de la heurística, sumar el número de coches que bloquean el camino de 'A' hacia la salida (ya que necesariamente habrá
-- que mover como mínimo esos vehículos) y el propio movimiento de 'A' (ya que al menos un movimiento es necesario para que 'A' llegue a la salida).
heuristic :: Board -> Int
  -- PRE: El coche 'A' debe existir en el tablero y estar orientado horizontalmente.
  -- POST: Devuelve el número de coches que bloquean el camino de 'A' hacia la salida más 1 (el movimiento de 'A' hacia la salida).
heuristic cars = length blockingCars + 1
  where
    pos = case find ((== 'A') . carId) cars of
      Just (Car _ p Horizontal) -> p
      Just (Car _ _ Vertical)   -> error "El coche 'A' no está orientado horizontalmente."
      Nothing                   -> error "No se encontró el coche 'A' en el tablero."
    rowA = fst (head pos)
    colMaxA = maximum (map snd pos)
    -- Filtra los coches que están en la misma fila que 'A' y a la derecha de 'A'
    blockingCars = [v | v <- cars, carId v /= 'A', any (\(r, c) -> r == rowA && c > colMaxA) (positions v)]

isSolved :: Board -> Bool
-- PRE: El coche 'A' debe existir en el tablero y estar orientado horizontalmente.
-- POST: Devuelve True si el coche 'A' ha llegado a la posición de salida, False en caso contrario.
isSolved cars =
  case find ((== 'A') . carId) cars of
    Just v -> any ((== 5) . snd) (positions v) -- Comprueba si alguna posición de 'A' está en la columna 5
    Nothing -> False

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapStr] ->
      let board = parseMap mapStr
       in
        -- Comenzar el algoritmo A* con el tablero y la heurística definida 
        case aStar getLegalMoves isSolved heuristic board of
            -- En caso de que no se encuentre solución, mostrar un mensaje de error 
            Nothing -> error "No se encontró solución para este mapa."

            -- En caso de que se encuentre solución, mostrar los pasos y la clasificación de dificultad
            Just solution -> do
              runVisualizer solution
    _ -> do
      putStrLn "Uso: cabal run Rush-Hour.cabal -- \"<mapa>\""
      exitFailure