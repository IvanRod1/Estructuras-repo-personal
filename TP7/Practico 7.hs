{-heapSort :: Ord a => [a] -> [a]
heapSort xs = pqToList (listToPq xs) 

listToPq :: Ord a => [a] -> PriorityQueue a
listToPq [] = emptyPQ                        --O(insertPQ x xs) * O(n) + O(emptyPQ) = O(n2)
listToPq (x:xs) = insertPQ x (listToPq xs)

pqToList :: PriorityQueue a -> [a]
pqToList pq = findMin pq : pqToList (deleteMin pq) O(n * n) = O(n2)

-}

--insertPQ x PQ = O(log n)

--O(log n) * O (n) = O(n log n)



----------------------------------------------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

bst1 :: Tree Int 
bst1 = (NodeT 8 (NodeT 3 (NodeT 1 EmptyT EmptyT) (NodeT 6 (NodeT 4 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))) (NodeT 10 EmptyT (NodeT 14 (NodeT 13 EmptyT EmptyT) EmptyT)))


belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _ EmptyT = False 
belongsBST x (NodeT y t1 t2) = if x == y then True else if x > y then belongsBST x t2 else belongsBST x t1 -- O(log N) donde n es la cantidad de nodos de una rama del arbol 

----------------------------------------------------------------------------------------------------------------------------------------------------------------

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT = (NodeT x EmptyT EmptyT)                                                                        --O(log N) donde n es la cantidad de elementos de una rama del arbol
insertBST x (NodeT y t1 t2) = if x == y then (NodeT y t1 t2) else if x > y then (NodeT y t1 (insertBST x t2)) else (NodeT y (insertBST x t1) t2)

---------------------------------------------------------------------------------------------------------------------------------------------------------------

deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y t1 t2) = if x == y then rearmarBST t1 t2 else if x > y then (NodeT y t1 (deleteBST x t2)) else (NodeT y (deleteBST x t1) t2)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
 -- PRECOND: ambos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti td = NodeT (maxBST ti) (delMaxBST ti) td

maxBST :: Tree a -> a
maxBST (NodeT x _ EmptyT) = x -- PRECOND: no es vacío
maxBST (NodeT _ _ td) = maxBST td    -- O(log n)

delMaxBST :: Tree a -> Tree a
delMaxBST (NodeT _ ti EmptyT) = ti -- PRECOND: no es vacío
delMaxBST (NodeT x ti td) = NodeT x ti (delMaxBST td) --O(log n)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Precondicion: el arbol debe ser BST

splitMinBST (NodeT x t1 t2) = (minBST t1, deleteMinBST t1) -- (O(log n) + O(log n)) = O(log n)

minBST :: Ord a => Tree a ->  a
minBST (NodeT x EmptyT _) = x           
minBST (NodeT x t1 t2) = minBST t1   --O(log n) + O(1) = O(log n), ya que en el peor caso, recorro una sola rama del arbol


deleteMinBST :: Tree a -> Tree a 
--precondicion: el arbol no debe ser vacio
deleteMinBST (NodeT _ EmptyT t2) = t2
deleteMinBST (NodeT x t1 t2) = NodeT x (deleteMinBST t1) t2  --O (log n), ya que en en el peor caso, recorre una rama del arbol y n es la cantidad de elementos de ese arbol

-----------------------------------------------------------------------------------------------------------------------------------------------------------------

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Precondicion: el arbol debe ser BST
splitMaxBST (NodeT x t1 t2) = (maxBST t2, delMaxBST t2)  --O(log n)

----------------------------------------------------------------------------------------------------------------------------------------------------------------

{-esBST :: Tree a -> Bool
esBST EmptyT = False
esBST (NodeT x t1 t2) = sonMenoresA x ti && sonMayoresA x td && (esBST ti) && (esBST td)

{-Invariante de BST: en (NodeT x ti td)
❏ todos los elementos de ti son menores que x
❏ todos los elementos de td son mayores que x       --O(N2)
❏ ti y td también cumplen el invariante de BST-}-

sonMenoresA :: Ord a => a -> Tree a -> Bool 
sonMenoresA _ EmptyT = False
sonMenoresA x (NodeT y t1 t2) = x >= y && (sonMenoresA x t1) && (sonMenoresA x t2) 

sonMayoresA :: Ord a => a -> Tree a -> Bool 
sonMayoresA _ EmptyT = False            
sonMayoresA x (NodeT y t1 t2) = x <= y && (sonMayoresA x t1) && (sonMayoresA x t2)-}

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
{-Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
elemento dado.
Costo: O(log N)-}
{-elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x (NodeT y t1 t2) = if x > y && esElMaximoMenor y t1 t2 then (Just y) else if x < y then elMaximoMenorA x t1 else elMaximoMenorA x t2-}

elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x t1 = if belongsBST x t1 then Just (elemento(arbolMenor(findBST x t1))) else buscarMaximoMenor x t1

esElMaximoMenor :: Ord a => a -> Tree a -> Tree a->Bool
esElMaximoMenor  x _ EmptyT = True 
esElMaximoMenor  x t1 t2 = x > maxBST t2 --O(log n)

esArbolVacio :: Tree a -> Bool
esArbolVacio EmptyT = True
esArbolVacio _ = False

findBST :: Ord a => a -> Tree a -> Tree a
findBST _ EmptyT = EmptyT      --O(log n)
findBST x (NodeT y t1 t2) = if x == y then (NodeT x t1 t2) else if x < y then findBST x t1 else findBST x t2


elemento :: Tree a -> a  --O(1)
elemento (NodeT x _ _) = x

arbolMenor :: Tree a -> Tree a --O(1)
arbolMenor (NodeT _ t1 _) = t1 

arbolMayor :: Tree a -> Tree a --O(1)
arbolMayor (NodeT _ _ t2) = t2


buscarMaximoMenor :: Ord a => a -> Tree a -> Maybe a 
buscarMaximoMenor _ EmptyT = Nothing        --O(log n) + O(log n) = O(log n)
buscarMaximoMenor x (NodeT y t1 t2) = if x > y && esElMaximoMenor y t1 t2 then (Just y) else if x < y then buscarMaximoMenor x t1 else buscarMaximoMenor x t2

-------------------------------------------------------------------------------------------------------------------------

{-elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
{-Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
elemento dado.
Costo: O(log N)-}

elMinimoMayorA x t1 = if belongsBST x t1 then (Just elemento(arbolMayor t1)) else buscarMinimoMayor a t1

buscarMinimoMayor :: Ord a => a -> Tree a -> Maybe a
buscarMinimoMayor _ EmptyT = 
buscarMinimoMayor x (NodeT y t1 t2) = if x < y && esElMinimoMayor y t1 t2 then (Just y) else if x < y then -}

-----------------------------------------------------------------------------------------------------------------------------

{-emptyM :: Map k v
Costo: O(1). Porque esta funcion siempre va a devolver un map vacio-}

{-assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K). K es la cantidad de veces que se tiene que repetir la funcion para contemplar todos los casos (en promedio) (en el peor caso es O(K))-}

{-deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K). K es la cantidad de veces que se tiene que repetir la funcion para contemplar todos los casos (en promedio) (en el peor caso es O(K))-}

{-keys :: Map k v -> [k]
Costo: O(K). K es la cantidad de claves map, por ende la funcion se va a repetir tantas K haya-} 

---------------------------------------------------------------------------------------------------------------------------
-- Para cada empleado e, valor de mp2, pertenece a algun conjunto valor de mp1 --es pisada por la segunda
-- Para cada empleado e, valor de mp2 ,los elementos de ' sectores e' son claves de mp1 y el empleado e pertenece a los conjuntos asociados 
-- Para cada CUIL c, clave de mp2; para el empleado e asociado a c, 'cuil e' es igual a c
-- Para cada empleado e, que pertenece a algun conjunto de valores de mp1, e esta asociado a un CUIL en mp2
