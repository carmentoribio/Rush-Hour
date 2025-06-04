module AStar where

import Data.List (sortOn, find)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
-- Definición de tipos
type Position = (Int, Int) -- (fila, columna)

data Orientation = Horizontal | Vertical deriving (Show, Eq, Ord)

data Car = Car
  { carId :: Char,             -- Identificador del coche (A-Z)
    positions :: [Position],   -- Lista de posiciones ocupadas por el coche
    orientation :: Orientation -- Orientación del coche (Horizontal o Vertical)
  }
  deriving (Show, Eq, Ord)


dfs ::
  (Ord a) =>
  (a -> [a]) -> -- Función que genera los nodos hijos
  (a -> Bool) -> -- Función que verifica si un nodo es el objetivo
  a -> -- Nodo inicial
  Maybe [a] -- Resultado de la búsqueda
dfs getChildren isGoal start = step Set.empty [(start, [start])]
  where
    step _ [] = Nothing
    step visited ((x, path) : xs)
      | isGoal x = Just (reverse path) -- Invertimos el camino para devolverlo en el orden correcto
      | x `Set.member` visited = step visited xs
      | otherwise =
          let children = [(y, y:path) | y <- getChildren x] -- guardamos los hijos con el camino actual en orden inverso por eficiencia
           in step (Set.insert x visited) (children ++ xs)

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

{- FIXME: basura que ha dado PErplejo machaca cosas, la dejo por si ves algo que te sirva dentro
moveVehicle :: [Car] -> Car -> [[Car]]
moveVehicle board car =
  [ movedCar : others
  | delta <- allowedMovements (length (positions car))
  , Just movedCar <- [tryMove delta car]
  ]
  where
    others = filter (/= car) board
    occupied = concatMap positions others

    tryMove :: Int -> Car -> Maybe Car
    tryMove delta v =
      let shift = if orientation v == Horizontal then \(r, c) d -> (r, c + d)
                                              else \(r, c) d -> (r + d, c)
          newPositions = map (\pos -> shift pos delta) (positions v)
          -- Calcula el trayecto que debe estar libre
          entry = if delta > 0 then last (positions v) else head (positions v)
          path = [ shift entry d | d <- steps delta ]
          isValid = all inBounds newPositions && all (`notElem` occupied) path
      in if isValid then Just v { positions = newPositions } else Nothing

    -- Genera los pasos intermedios del trayecto
    steps d | d > 0     = [1..d]
            | d < 0     = [d..(-1)]
            | otherwise = []

    inBounds (r, c) = r >= 0 && r < 6 && c >= 0 && c < 6

    -- Puedes ajustar los movimientos permitidos según el tamaño del coche
    allowedMovements carLen = filter (\d -> abs d < 6) [4,3,2,1,-1,-2,-3,-4]
-}


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
          isValid = all inBounds newPos && all (`notElem` occupied) newPos --FIXME: hay que comprobar que no te saltas ningún otro coche
      in (if isValid then Just newPos else Nothing, v { positions = newPos })

    inBounds (f, c) = f >= 0 && f < 6 && c >= 0 && c < 6 -- Verifica que las posiciones estén dentro del tablero 6x6

isSolved :: [Car] -> Bool
-- PRE: El coche 'A' debe existir en el tablero y estar orientado horizontalmente.
-- POST: Devuelve True si el coche 'A' ha llegado a la posición de salida, False en caso contrario.
isSolved cars =
  case find (\v -> carId v == 'A' && orientation v == Horizontal) cars of
    Just v  -> any ((== 5) . snd) (positions v) -- Comprueba si alguna posición de 'A' está en la columna 5
    Nothing -> False

-- A* usando listas como frontera
aStar ::
  (Ord state)
  => (state -> Bool)         -- ^ ¿Es este estado el objetivo?
  -> (state -> [state])      -- ^ Sucesores de un estado
  -> (state -> Int)          -- ^ Heurística
  -> state                   -- ^ Estado inicial
  -> Maybe [state]           -- ^ Camino solución (o Nothing)
-- PRE: La función `isGoal` debe ser una función que verifica si un estado es el objetivo. La heuristica debe ser válida y no negativa para todos los estados.
-- POST: Devuelve un camino desde el estado inicial hasta el objetivo, o Nothing si no hay solución.
aStar isGoal successors heuristic initial = search Set.empty [(initial, 0, heuristic initial, [initial])]
  where
    -- La frontera es una lista de tuplas: (estado, g, f, camino)
    -- g: coste real hasta aquí, f: g + h, camino: desde inicial hasta aquí
    search _ [] = Nothing  -- Si la frontera está vacía, no hay solución

    search visited ((current, g, f, path):frontier)
      | isGoal current = Just (reverse path)
      | Set.member current visited = search visited frontier
      | otherwise =
          let visited' = Set.insert current visited
              -- Genera sucesores que no han sido visitados
              children = [ (child, g+1, g+1 + heuristic child, child:path)
                         | child <- successors current
                         , Set.notMember child visited'
                         ]
              -- Añade los hijos a la frontera y la ordena por f (coste estimado)
              newFrontier = insertAllSorted children frontier
          in search visited' newFrontier

    -- Inserta una lista de nodos en la frontera y la ordena por f
    insertAllSorted xs ys = sortOn (\(_,_,f,_) -> f) (xs ++ ys)
