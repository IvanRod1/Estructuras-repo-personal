head' :: [a] -> a
head' (x:xs) = x

--Constante O(1)

-----------------------
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

--Constante O(1)

--------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--Lineal O(n)

--------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Lineal O(n)

---------------------------------------------

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- Cuadratica 

----------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

--Lineal O(n)

------------------------------------------------

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =   if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

--Cuadratica 

------------------------------------------------

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Lineal

------------------------------------------

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

--Lineal

-------------------------------------------

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

--Lineal

---------------------------------------------------

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

--Lineal

----------------------------------------------------

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

--Cuadratica?

{-Cubica : Por cada elemento de la estructura, se hacen operaciones
de costo cuadrático-}

-------------------------------------------------------------

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

--Cuadratica
-------------------------------------------------------------------

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                 then xs
                 else x : sacar n xs

--lineal

----------------------------------------------------------------------

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
            let m = minimo xs
            in m : ordenar (sacar m xs)

--Cuadratica ?? o Cubica??

-----------------------------------------------------------------------





