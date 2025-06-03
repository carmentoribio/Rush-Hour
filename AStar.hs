module AStar where

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
      | isGoal x = Just path
      | x `Set.member` visited = step visited xs
      | otherwise =
          let children = [(y, path ++ [y]) | y <- getChildren x]
           in step (Set.insert x visited) (children ++ xs)