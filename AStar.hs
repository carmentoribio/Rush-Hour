module AStar where

import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set



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

-- A* usando listas como frontera
aStar
  :: (Ord state)
  => (state -> Bool)         -- ^ ¿Es este estado el objetivo?
  -> (state -> [state])      -- ^ Sucesores de un estado
  -> (state -> Int)          -- ^ Heurística
  -> state                   -- ^ Estado inicial
  -> Maybe [state]           -- ^ Camino solución (o Nothing)
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
