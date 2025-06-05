module AStar where

import Data.List (sortOn)
import Data.Set qualified as Set

-- A* usando listas como frontera
aStar ::
  (Ord a) =>
  (a -> [a]) -> -- Función que genera los nodos hijos
  (a -> Bool) -> -- Función que verifica si un nodo es el objetivo
  (a -> Int) -> -- Heurística para estimar el coste desde un estado hasta el objetivo
  a -> -- Nodo inicial
  Maybe [a] -- Resultado de la búsqueda
  -- PRE: La función `isGoal` debe ser una función que verifica si un estado es el objetivo. La heuristica debe ser válida y no negativa para todos los estados.
  -- POST: Devuelve un camino desde el estado inicial hasta el objetivo, o Nothing si no hay solución.
aStar getChildren isGoal heuristic start = search Set.empty [(start, 0, heuristic start, [start])]
  where
    -- La frontera es una lista de tuplas: (estado, g, f, camino)
    -- g: coste real hasta aquí, f: g + h, camino: desde inicial hasta aquí
    search _ [] = Nothing -- Si la frontera está vacía, no hay solución
    search visited ((current, g, _, path) : frontier)
      | isGoal current = Just (reverse path) -- Invertimos el camino para devolverlo en el orden correcto
      | Set.member current visited = search visited frontier
      | otherwise =
          let visited' = Set.insert current visited
              -- Genera sucesores que no han sido visitados
              children =
                [ (child, g + 1, g + 1 + heuristic child, child : path)
                  | child <- getChildren current,
                    Set.notMember child visited'
                ]
              -- Añade los hijos a la frontera y la ordena por f (coste estimado)
              newFrontier = insertAllSorted children frontier
           in search visited' newFrontier

    -- Inserta una lista de nodos en la frontera y la ordena por f
    insertAllSorted xs ys = sortOn (\(_, _, f, _) -> f) (xs ++ ys)