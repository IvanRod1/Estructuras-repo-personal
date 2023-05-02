import PriorityQueue
import Map

{-instance Eq Persona Where
  p1 == p2 = edad p1 == edad p2
                                  ejemplo persona fidel
  instance Ord Persona where
  p1 <= p2 = edad p1 <= edad p2-}

{-heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort (x:xs) = if x < minimum xs
                  then x : heapSort xs
                  else agregarAlFinal x (heapSort xs)-}
----------------------------------------------------------------------
{-Map-}
--1
valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.

valuesM m = if esMapVacio m
            then [Nothing]
            else valorDeLasKeys (keys m) m 

valorDeLasKeys :: Eq k => [k] -> Map k v -> [v]
valorDeLasKeys k m = lookupM k (head m) ++ lookupM k (tail kvs)

esMapVacio :: Map k v -> Bool
esMapVacio m = null (keys m)

--2
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.

todasAsociadas ks m = comparadorDeKeys ks (keys m)

comparadorDeKeys :: Eq a => [a] -> [a] -> Bool
comparadorDeKeys (x:xs) ys = contiene x ys && comparadorDeKeys xs ys


