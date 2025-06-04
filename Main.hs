module Main where

import AStar ( aStar )
import Data.Map qualified as Map
import Data.List (find)

{-
Ideas generales:
- Funcion que resuleva el tablero, con un contador de movimientos. Debe ser la solucion minima (usar A* con permutaciones del mapa cada nodo)
- Funcion que cuenta el numero de coches distintos
- Funcion que mida la simetria (????)
- Funcion que dado el numero de movimientos, el numero de coches distintos, el factor de simetria etc, devuelva un valor de dificultad. (ajustar coeficientes del polinomio hasta obtener una solucion con sentido)
-}

-- Definición de tipos
type Position = (Int, Int) -- (fila, columna)

data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord)

data Car = Car
  { carId :: Char,
    positions :: [Position],
    orientation :: Orientation
  }
  deriving (Show, Eq, Ord)

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
        [v | v <- cars, carId v /= 'A', any (\(r, c) -> r == rowA && c > colMaxA) (positions v)]
   in length blockingCars + 1 -- +1 para contar el movimiento de 'A' hacia la salida
  where
    Just (Car _ pos Horizontal) = find ((== 'A') . carId) cars -- encontrar el coche A

-- Desplazamientos posibles: + (adelante), - (atrás), vehículos más pequeños de 2 casillas se pueden desplazar máximo 4 casillas
movements :: [Int]
movements = [4, 3, 2, 1, -1, -2, -3, -4]

moveVehicle :: [Car] -> Car -> [[Car]]
moveVehicle board car =
  [ newVehicle : others
  | delta <- movements
  , let (maybeNewPositions, newVehicle) = moveIfPossible delta car others
  , Just pos <- [maybeNewPositions]
  ]
  where
    others = filter (/= car) board
    occupied = concatMap positions others

    moveIfPossible :: Int -> Car -> [Car] -> (Maybe [Position], Car)
    moveIfPossible delta v _ =
      let pos = positions v
          newPos = case orientation v of
            Horizontal -> map (\(f, c) -> (f, c + delta)) pos
            Vertical   -> map (\(f, c) -> (f + delta, c)) pos
          isValid = all inBounds newPos && all (`notElem` occupied) newPos
      in (if isValid then Just newPos else Nothing, v { positions = newPos })

    inBounds (f, c) = f >= 0 && f < 6 && c >= 0 && c < 6

isSolved :: [Car] -> Bool
isSolved cars =
  case find (\v -> carId v == 'A' && orientation v == Horizontal) cars of
    Just v  -> any ((== 5) . snd) (positions v)
    Nothing -> False


inicialState :: [Car]
inicialState = parseMap "BBBooCooDooCAADooCEoDoFFEoooGoHHHoGo"

sucessors :: [Car] -> [[Car]]
sucessors state = concatMap (moveVehicle state) state

solution :: Maybe [[Car]]
solution = aStar isSolved sucessors heuristic inicialState

main :: IO ()
main = case solution of
  Nothing -> putStrLn "No se encontró solución."
  Just camino -> mapM_ print camino
