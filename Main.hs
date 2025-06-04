module Main where

import AStar (Car, Position, Orientation(..), carId, positions)
import Parse (parseMap, showBoard, solution)
import Difficulty (classifyDifficulty)
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