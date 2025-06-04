module Parse where
import Data.Map qualified as Map
import Data.List (find)
import AStar (Car(..), Position, Orientation(..), aStar, isSolved, heuristic, moveVehicle)
import Data.List (intercalate)

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

-- FIXME: todo esto son renombrados, los he dajdo por dejar, si quieres percutetelos
testState :: [Car]
testState = parseMap "BBBooCooDooCAADooCEoDoFFEoooGoHHHoGo"

sucessors :: [Car] -> [[Car]] -- FIXME: me da la sensación de que va a dar error, epro lo dejo por si acaso
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