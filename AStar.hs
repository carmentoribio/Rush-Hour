module AStar where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

dfs ::
  (Ord a) =>
  (a -> [a]) -> -- Función que genera los nodos hijos
  (a -> Bool) -> -- Función que verifica si un nodo es el objetivo
  a -> -- Nodo inicial
  Maybe a -- Resultado de la búsqueda
dfs getChildren isGoal = step Set.empty
  where
    step visited current
      | isGoal current = Just current
      | Set.member current visited = Nothing
      | otherwise =
          let visited' = Set.insert current visited
              children = getChildren current
           in tryChildren visited' children
    tryChildren visited' [] = Nothing
    tryChildren visited' (child : rest) =
      case step visited' child of
        Just result -> Just result
        Nothing -> tryChildren visited' rest