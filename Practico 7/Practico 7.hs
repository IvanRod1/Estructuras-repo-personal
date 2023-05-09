{-Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que
el usuario utiliza una priority queue con costos logarítmicos de inserción y borrado (o sea, usa una
Heap como tipo de representación).-}
{-data PriorityQueue a = PQ [a] Tree a


O(n) en el peor caso o O(log n) en promedio-}

-------------------

belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
--Precondicion: el arbol es BST
belongsBST _ EmptyT = False
belongsBST x (NodeT y ti td) = if (x == y)
                               then True
                               else if (x < y)
                                then belongsBST x ti
                               else belongsBST x td

insertBST :: Ord a => a -> Tree a -> Tree a
{-Propósito: dado un BST inserta un elemento en el árbol.
Costo: O(log N)-}

insertBST x emptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y
                              then NodeT x ti td
                              else if x < y
                                then NodeT y (insertBST x ti) td
                              else NodeT y ti (insertBST x td)

deleteBST :: Ord a => a -> Tree a -> Tree a
{-Propósito: dado un BST borra un elemento en el árbol.
Costo: O(log N)-}
deleteBST x EmptyT =
deleteBST x (NodeT y ti td) = if x == y 
                              then rearmarBST ti td
                              else if x < y
                                then NodeT y (deleteBST x ti) td
                              else NodeT y ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
{-Precondicion: ambos arboles son BST-} 
rearmarBST EmptyT td = td
rearmarBST ti td = let (m,tii) = splitMax ti
                   in NodeT m tii td

splitMax :: Ord a => Tree a -> (a, Tree a)
{-Precondicion: El arbol es BST y no es vacio-}
splitMax (NodeT x ti EmptyT) = (x,ti)
splitMax (NodeT x ti td) = let (m,tdd) = splitMax td
                           in (m, NodeT x ti tdd)


splitMin :: Ord a => Tree a -> (a, Tree a)
{-Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
Costo: O(log N)-}
{-Precondicion: El arbol es BST y no es vacio-}
splitMin (NodeT x EmptyT td) = (x,td)
splitMin (NodeT x ti td) = let (m,tii) = splitMin td
                           in (m, NodeT x tii td)

esBST :: Tree a -> Bool
{-Propósito: indica si el árbol cumple con los invariantes de BST.
Costo: O(N2) -}
esBST EmptyT = False
esBST (NodeT x ti td) = todosSonMenoresA x ti && todosSonMayoresA x td

todosSonMenoresA :: Ord a => a -> Tree a -> Bool --O(n)
{-Precondicion:..-}
todosSonMenoresA _ EmptyT = False
todosSonMenoresA x (NodeT y ti td) = x < y && todosSonMenoresA x ti && todosSonMenoresA x td


todosSonMayoresA :: Ord a => a -> Tree a -> Bool  --O(n)
{-Precondicion:..-}
todosSonMayoresA _ EmptyT = False
todosSonMayoresA x (NodeT y ti td) = x > y && todosSonMayoresA x ti && todosSonMayoresA x td


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
{-Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
elemento dado.
Costo: O(log N)-}

elMaximoMenorA x EmptyT = Nothing
elMaximoMenorA x (NodeT y ti _) = elMaximoMenorA x ti
elMaximoMenorA x (NodeT y EmptyT _) = Just y 


elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
{-Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
elemento dado.
Costo: O(log N)-}

elMinimoMayorA x EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) = elMinimoMayorA x td
elMinimoMayorA x (NodeT y _ td) = Just y


balanceado :: Tree a -> Bool
{-Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
Costo: O(N2)-}

balanceado EmptyT = False
balanceado (NodeT _ ti td) = (heightT ti - heightT td) <= 1





