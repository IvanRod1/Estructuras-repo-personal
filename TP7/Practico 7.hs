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
esBST EmptyT = 
esBST (NodeT x t1 t2) = -}

{-Invariante de BST: en (NodeT x ti td)
❏ todos los elementos de ti son menores que x
❏ todos los elementos de td son mayores que x
❏ ti y td también cumplen el invariante de BST-}

{-sonMenoresA :: Ord a => a -> Tree a -> Bool 
sonMenoresA _ EmptyT = False
sonMenoresA x (NodeT y t1 _) = x > y && (sonMenoresA y t1) 

sonMayoresA :: Ord a => a -> Tree a -> Bool 
sonMayoresA _ EmptyT = False 
sonMayoresA x (NodeT y _ t2) = x < y && (sonMayoresA y t2)-}

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
{-Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
elemento dado.
Costo: O(log N)-}
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x (NodeT y t1 t2) = if x > y && esElMaximoMenor y t1 t2 then (Just y) else if x < y then elMaximoMenorA x t1 else elMaximoMenorA x t2

esElMaximoMenor :: Ord a => a -> Tree a -> Tree a -> Bool
esElMaximoMenor  x _ EmptyT = True 
esElMaximoMenor  x ti td = x > maxBST td  

esArbolVacio :: Tree a -> Bool
esArbolVacio EmptyT = True
esArbolVacio _ = False


