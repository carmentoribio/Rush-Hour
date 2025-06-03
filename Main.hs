module Main where

import AStar
import Data.Map qualified as Map

{-
Ideas generales:
- Funcion que resuleva el tablero, con un contador de movimientos. Debe ser la solucion minima (usar A* con permutaciones del mapa cada nodo)
- Funcion que cuenta el numero de coches distintos
- Funcion que mida la simetria (????)
- Funcion que dado el numero de movimientos, el numero de coches distintos, el factor de simetria etc, devuelva un valor de dificultad. (ajustar coeficientes del polinomio hasta obtener una solucion con sentido)
-}

-- Definición de tipos
type Position = (Int, Int) -- (fila, columna)

data Orientation = Horizontal | Vertical deriving (Show, Eq)

data Car = Car
  { id :: Char,
    positions :: [Position],
    orientation :: Orientation
  }
  deriving (Show, Eq)

-- TODO: actualemnte inutil, si no seusa borrar
newtype Board = Board {cars :: [Car]} deriving (Show, Eq)

-- TODO: Comprobar que el mapa es correcto
-- Parser de mapa a lista de coches
parseMap :: String -> [Car]
parseMap str =
  let positions = [(i `div` 6, i `mod` 6, c) | (i, c) <- zip [0 ..] str, c /= 'o']
      grouped = Map.fromListWith (++) [(c, [(row, col)]) | (row, col, c) <- positions]
      cars =
        [ Car carId posList (getOrientation posList)
          | (carId, posList) <- Map.toList grouped
        ]
   in cars

getOrientation :: [Position] -> Orientation
getOrientation pos
  | all ((== head rows) . fst) pos = Horizontal
  | all ((== head cols) . snd) pos = Vertical
  | otherwise = error "Vehículo mal formado"
  where
    (rows, cols) = unzip pos

-- Para el cálculo de la heurística, sumar el número de coches que bloquean el camino de 'A' hacia la salida (ya que necesariamente habrá
-- que mover como mínimo esos vehículos) y el propio movimiento de 'A' (ya que al menos un movimiento es necesario para que 'A' llegue a la salida).
heuristic :: [Car] -> Int
heuristic cars =
  -- Calcula los coches que bloquean el camino de 'A' hacia la salida (los coches que están en la misma fila y a la derecha de 'A')
  let rowA = fst (head pos)
      colMaxA = maximum (map snd pos)
      blockingCars =
        [v | v <- cars, carId v /= 'A', any (\(r, c) -> r == rowA && c > colMaxA) (carPositions v)]
   in length blockingCars + 1 -- +1 para contar el movimiento de 'A' hacia la salida
  where
    (Car _ pos Horizontal) = find ((== 'A') . carId) cars -- encontrar el coche A

main :: IO ()
main = do
  putStrLn "Hola"