{-
head' :: [a] -> a
head' (x:xs) = x cons

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 lineal

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) cuadratico

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs lineal

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs cubico 

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs   lineal

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
                    if pertenece x xs
                    then sinRepetidos xs
                    else x : sinRepetidos xs  cuadratica           

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys  lineal

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs lineal

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs cuadratico

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs lineal

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
                if n == x
                then xs
                else x : sacar n xs

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs in m : ordenar (sacar m xs)

-}
import SetV1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show



losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = if belongs x s 
                             then x : losQuePertenecen xs s
                             else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if belongs x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs

unirTodos :: Eq a => Tree (Set a) -> Set a 
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos NodeT EmptyT = Set []
unirTodos (NodeT ((Set xs) t1 t2)) = Set (xs ++ unirTodos t1 ++ unirTodos t2)

