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
dfs next isGoal start = go Set.empty [start]
  where
    go _ [] = Nothing
    go visited (x:xs)
      | isGoal x = Just x
      | x `Set.member` visited = go visited xs
      | otherwise = go (Set.insert x visited) (next x ++ xs)