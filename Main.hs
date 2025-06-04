module Main where

import AStar ( aStar )
import Data.Map qualified as Map
import Data.List (find, intercalate)

import System.Environment (getArgs)
import System.Exit (exitFailure)

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
  { carId :: Char,             -- Identificador del coche (A-Z)
    positions :: [Position],   -- Lista de posiciones ocupadas por el coche
    orientation :: Orientation -- Orientación del coche (Horizontal o Vertical)
  }
  deriving (Show, Eq, Ord)

-- TODO: actualemnte inutil, si no seusa borrar
newtype Board = Board {cars :: [Car]} deriving (Show, Eq)

-- Estructura para métricas de solución
data SolutionMetrics = SolutionMetrics
  { steps :: Int          -- Número de movimientos en solución óptima
  , movedCars :: Int      -- Cantidad de coches diferentes movidos
  , carNumber :: Int      -- Cantidad total de coches en el tablero
  , symmetryScore :: Int  -- Grado de simetría (0-100)
  } deriving (Show)


-- FIXME: si quieres añadir más casos de error ponlos como comprobaciones como estos dos, si necesitas funciones auxiliares metelas en el where
-- Parser de mapa a lista de coches
parseMap :: String -> [Car]
-- PRE: El mapa debe tener exactamente 36 caracteres (6x6) y solo contener mayúsculas (A-Z) y 'o' (espacio vacío).
-- POST: Devuelve una lista de coches con sus posiciones y orientaciones.
parseMap str
  | length str /= 36 = error "El mapa debe tener exactamente 36 caracteres (6x6)."
  | not (all validChar str) = error "El mapa contiene caracteres no válidos (solo mayúsculas y 'o' permitidos)."
  | otherwise =
  let positions = [(i `div` 6, i `mod` 6, c) | (i, c) <- zip [0 ..] str, c /= 'o']      -- (fila, columna, caracter) para cada posición ocupada
      grouped = Map.fromListWith (++) [(c, [(row, col)]) | (row, col, c) <- positions]  -- Agrupa posiciones por coche
      cars =
        [ Car carId posList (getOrientation posList)        -- Crea un coche con su ID, posiciones y orientación
          | (carId, posList) <- Map.toList grouped
        ]
   in cars
  where
    validChar c = c == 'o' || (c >= 'A' && c <= 'Z')

getOrientation :: [Position] -> Orientation
-- PRE: Las posiciones deben pertenecer al mismo coche y estar en una fila o columna continua.
-- POST: Devuelve la orientación del coche (Horizontal o Vertical).
getOrientation pos
  | all ((== head rows) . fst) pos = Horizontal -- Todas las filas son iguales, orientación horizontal
  | all ((== head cols) . snd) pos = Vertical   -- Todas las columnas son iguales, orientación vertical
  | otherwise = error "Vehículo mal formado"
  where
    (rows, cols) = unzip pos

-- Para el cálculo de la heurística, sumar el número de coches que bloquean el camino de 'A' hacia la salida (ya que necesariamente habrá
-- que mover como mínimo esos vehículos) y el propio movimiento de 'A' (ya que al menos un movimiento es necesario para que 'A' llegue a la salida).
heuristic :: [Car] -> Int
heuristic cars =
        -- PRE: El coche 'A' debe existir en el tablero y estar orientado horizontalmente.
        -- POST: Devuelve el número de coches que bloquean el camino de 'A' hacia la salida más 1 (el movimiento de 'A' hacia la salida).
  let rowA = fst (head pos)
      colMaxA = maximum (map snd pos)
      blockingCars =
        [v | v <- cars, carId v /= 'A', any (\(r, c) -> r == rowA && c > colMaxA) (positions v)] -- Filtra los coches que están en la misma fila que 'A' y a la derecha de 'A'
   in length blockingCars + 1
  where
    Just (Car _ pos Horizontal) = find ((== 'A') . carId) cars -- encontrar el coche A

movements :: [Int]
-- Desplazamientos posibles: + (adelante), - (atrás), vehículos más pequeños de 2 casillas se pueden desplazar máximo 4 casillas
movements = [4, 3, 2, 1, -1, -2, -3, -4]

moveVehicle :: [Car] -> Car -> [[Car]]
-- PRE: El coche debe estar en el tablero y las posiciones deben ser válidas.
-- POST: Devuelve una lista de tableros resultantes de hacer todos los movimientos posibles del coche.
moveVehicle board car =
  [ newVehicle : others -- Crea un nuevo tablero con el coche movido y los demás coches sin cambios
  | delta <- movements
  , let (maybeNewPositions, newVehicle) = moveIfPossible delta car others -- Intenta mover el coche en la dirección indicada por delta
  , Just pos <- [maybeNewPositions] -- Asegura que el movimiento es válido y devuelve las nuevas posiciones del coche
  ]
  where
    others = filter (/= car) board
    occupied = concatMap positions others

    moveIfPossible :: Int -> Car -> [Car] -> (Maybe [Position], Car)
    -- PRE: El coche debe estar en el tablero y las posiciones deben ser válidas.
    -- POST: Devuelve las nuevas posiciones del coche si el movimiento es válido, o Nothing si no es posible.
    moveIfPossible delta v _ =
      let pos = positions v
          newPos = case orientation v of -- Determina la nueva posición según la orientación del coche
            Horizontal -> map (\(f, c) -> (f, c + delta)) pos
            Vertical   -> map (\(f, c) -> (f + delta, c)) pos
                                        -- Verifica que las nuevas posiciones estén dentro de los límites y no ocupadas por otros coches
          isValid = all inBounds newPos && all (`notElem` occupied) newPos
      in (if isValid then Just newPos else Nothing, v { positions = newPos })

    inBounds (f, c) = f >= 0 && f < 6 && c >= 0 && c < 6 -- Verifica que las posiciones estén dentro del tablero 6x6

isSolved :: [Car] -> Bool
-- PRE: El coche 'A' debe existir en el tablero y estar orientado horizontalmente.
-- POST: Devuelve True si el coche 'A' ha llegado a la posición de salida, False en caso contrario.
isSolved cars =
  case find (\v -> carId v == 'A' && orientation v == Horizontal) cars of
    Just v  -> any ((== 5) . snd) (positions v) -- Comprueba si alguna posición de 'A' está en la columna 5
    Nothing -> False

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

-- FIXME: todo esto son renombrados, los he dajdo por dejar, si quieres percutetelos
testState :: [Car]
testState = parseMap "BBBooCooDooCAADooCEoDoFFEoooGoHHHoGo"

sucessors :: [Car] -> [[Car]]
sucessors state = concatMap (moveVehicle state) state

solution :: [Car] -> Maybe [[Car]]
solution = aStar isSolved sucessors heuristic -- Tienen aplicada la currificación o como se diga, se aplica a un estado incial

showBoard ::[Car] -> String
-- PRE: La lista de coches debe ser válida y contener al menos el coche 'A' en una posición válida.
-- POST: Devuelve una representación en string del tablero, donde 'o' representa espacios vacíos y las letras representan los coches.
showBoard cars =
  let
      posMap = Map.fromList [ (pos, carId car) | car <- cars, pos <- positions car ] -- Construye un mapa de posiciones a identificadores de coche
      showRow r = [ Map.findWithDefault 'o' (r, c) posMap | c <- [0..5] ] -- Genera cada fila del tablero usando 'o' como espacio vacío
      boardLines = [ showRow r | r <- [0..5] ] -- Junta las filas en un string
  in intercalate "\n" boardLines -- Muestra el tablero en formato de string

-- FIXME: La idea es que imprima los pasos y la clasificación de dificultad del mapa dado como argumento
main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapStr] ->
      let board = parseMap mapStr -- Aquí se puede usar la función solution para encontrar la solución óptima
      in case solution board of
            Nothing -> error "No se encontró solución para este mapa."
            Just solution -> do
              putStrLn "Solución paso a paso:"
              mapM_ (\b -> putStrLn (showBoard b) >> putStrLn "----") solution
              putStrLn $ "Clasificación de dificultad: " ++ classifyDifficulty mapStr --FIXME: se esta calculando la solución 2 veces
                                                        -- No sé si implementarle polimorfismo a la función
    _ -> do
      putStrLn "Uso: ./Main <mapa>"
      exitFailure