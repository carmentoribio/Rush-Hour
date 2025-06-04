module BoardUtils where

import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Set qualified as Set

-- ======================== Definición de tipos =========================
type Position = (Int, Int) -- (fila, columna)

data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord)

data Car = Car
  { carId :: Char, -- Identificador del coche (A-Z)
    positions :: [Position], -- Lista de posiciones ocupadas por el coche
    orientation :: Orientation -- Orientación del coche (Horizontal o Vertical)
  }
  deriving (Show, Eq, Ord)

type Board = [Car] -- Lista de coches en el tablero

-- ======================================================================

-- ================== Parser de mapa a lista de coches ==================
parseMap :: String -> Board
-- PRE: El mapa debe tener exactamente 36 caracteres (6x6) y solo contener mayúsculas (A-Z) y 'o' (espacio vacío).
-- POST: Devuelve una lista de coches con sus posiciones y orientaciones.
parseMap str
  | length str /= 36 = error "El mapa debe tener exactamente 36 caracteres (6x6)."
  | not (all validChar str) = error "El mapa contiene caracteres no válidos (debe indicar los coches con letras mayúsculas, y las posiciones vacías con el caracter 'o')."
  | otherwise =
      let positions = [(i `div` 6, i `mod` 6, c) | (i, c) <- zip [0 ..] str, c /= 'o'] -- (fila, columna, caracter) para cada posición ocupada
          grouped = Map.fromListWith (++) [(c, [(row, col)]) | (row, col, c) <- positions] -- Agrupa posiciones por coche
          cars =
            [ Car carId posList (getOrientation posList) -- Crea un coche con su ID, posiciones y orientación
              | (carId, posList) <- Map.toList grouped
            ]
       in normalizeBoard cars -- Normaliza el tablero ordenando los coches por su ID, para mantener un orden consistente
  where
    validChar c = c == 'o' || (c >= 'A' && c <= 'Z')

getOrientation :: [Position] -> Orientation
-- PRE: Las posiciones deben pertenecer al mismo coche y estar en una fila o columna continua.
-- POST: Devuelve la orientación del coche (Horizontal o Vertical).
getOrientation pos
  | all ((== head rows) . fst) pos = Horizontal -- Todas las filas son iguales, orientación horizontal
  | all ((== head cols) . snd) pos = Vertical -- Todas las columnas son iguales, orientación vertical
  | otherwise = error "Vehículo mal formado"
  where
    (rows, cols) = unzip pos

-- ======================================================================

-- =============================== Utiles ===============================

normalizeBoard :: Board -> Board
normalizeBoard = sortOn carId

moveVehicle :: Board -> Car -> [Board]
-- PRE: El coche 'car' debe estar en el tablero 'board' y las posiciones deben ser válidas.
-- POST: Devuelve una lista de tableros resultantes de hacer todos los movimientos válidos posibles del coche 'car'.
moveVehicle board car =
  let -- Obtiene las posiciones ocupadas por otros coches en el tablero
      occPos = occupiedPositions board `Set.difference` Set.fromList (positions car)

      -- Genera una lista de posiciones posibles para el coche:
      -- Mientras haya posiciones válidas, sigue generando movimientos
      -- 'dirFn' es una función que toma una posición y devuelve la nueva posición al mover el coche en una dirección específica
      generateMoves dirFn =
        takeWhile (isValidMove occPos) $
          tail $
            iterate (fmap dirFn) (positions car)

      -- Verifica si las posiciones generadas son válidas (dentro del tablero y no ocupadas por otros coches)
      isValidMove occPos posList =
        all inBounds posList && all (`Set.notMember` occPos) posList
      inBounds (r, c) = r >= 0 && r < 6 && c >= 0 && c < 6

      -- Crea un nuevo coche con las posiciones actualizadas
      newCars newPos = Car (carId car) newPos (orientation car)
   in -- Genera los tableros resultantes de mover el coche en todas las direcciones posibles
      case orientation car of
        -- En caso de ser horizontal, concatenamos todos los movimientos posibles hacia la izquierda y hacia la derecha, y generamos una lista de tableros
        Horizontal ->
          map (normalizeBoard . replaceCar board . newCars) $
            generateMoves (\(r, c) -> (r, c - 1)) -- izquierda
              ++ generateMoves (\(r, c) -> (r, c + 1)) -- derecha

        -- En caso de ser vertical, concatenamos todos los movimientos posibles hacia arriba y hacia abajo, y generamos una lista de tableros
        Vertical ->
          map (normalizeBoard . replaceCar board . newCars) $
            generateMoves (\(r, c) -> (r - 1, c)) -- arriba
              ++ generateMoves (\(r, c) -> (r + 1, c)) -- abajo
  where
    occupiedPositions = Set.fromList . concatMap positions
    replaceCar board newCar = newCar : filter ((/= carId newCar) . carId) board

getLegalMoves :: Board -> [Board]
-- PRE: El tablero 'board' debe tener más de un coche.
-- POST: Devuelve una lista de todos los tableros resultantes de realizar todos los posibles movimientos de cada coche del tablero 'board'.
getLegalMoves board = concatMap (moveVehicle board) board

showBoard :: Board -> String
-- PRE: La lista de coches debe ser válida y contener al menos el coche 'A' en una posición válida.
-- POST: Devuelve una representación en string del tablero, donde 'o' representa espacios vacíos y las letras representan los coches.
showBoard cars =
  let posMap = Map.fromList [(pos, carId car) | car <- cars, pos <- positions car]
   in unlines [[Map.findWithDefault 'o' (r, c) posMap | c <- [0 .. 5]] | r <- [0 .. 5]]

-- ======================================================================
