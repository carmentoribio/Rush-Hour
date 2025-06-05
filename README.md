# Rush Hour
## CONTEXTO

Rush Hour es un conocido rompecabezas de bloques deslizantes inventado por Nob Yoshigahara en la década de 1970. Simula un aparcamiento donde se aparcan coches (que ocupan dos posiciones) y camiones (que ocupan tres). El objetivo es conseguir que uno de los coches (siempre en la tercera fila) llegue a la salida, moviendo los otros vehículos. Un movimiento supone desplazar un vehículo una posición a la izquierda, a la derecha, arriba o abajo, si hay espacio para ello. Los vehículos no pueden rotar ni atravesar otros vehículos, y no pueden salir del aparcamiento. 

--------------------------------------------------------------------------------------

## OBJETIVO

Se desarrollará un programa que dada una situación inicial (comienzo del juego) la clasifique por su dificultad: principiante, intermedio, avanzado y experto.

Para ello se propondrá una fórmula (polinomio) en función de características de la solución mínima, por ejemplo, número de pasos, proporción de piezas movidas, grado de simetría, número de piezas en la situación inicial, etc.

--------------------------------------------------------------------------------------

### Leyenda del fichero RushHour.txt
En el fichero RushHour.txt se pueden encontrar diversos ejemplos de semillas de mapas resolubles. El formato de un mapa es el siguiente:
El aparcamiento es un tablero de 6 filas y 6 columnas.
Cada línea tiene una cadena con 36 caracteres que representan el tablero sin resolver. Cada 6 elementos representan una fila.
- o Posición vacía
- A Coche que mover
- B - Z Resto de vehículos

--------------------------------------------------------------------------------------

# Uso
En terminal, ejecutar `cabal build`. Una vez compilado, ejecutar `cabal run Rush-Hour.cabal -- "<mapa>"` para iniciar el programa, donde mapa será una cadena de 36 caracteres como las del fichero RushHour.txt.