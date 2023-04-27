module SetV1
(Set,emptyS,addS,belongs,sizeS,removeS, unionS, setToList) where

data Set a = Set [a]
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

{-Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
la cantidad de elementos en la estructura.
Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
de esta implementación, pero para mantener una interfaz común entre distintas posibles
implementaciones estamos obligados a escribir así los tipos.-}

--Sin repetidos

emptyS :: Set a 
addS :: Eq a => a -> Set a -> Set a 
belongs :: Eq a => a -> Set a -> Bool 
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]

emptyS = Set []

addS x (Set []) = Set [x]
addS x (Set xs) = Set (if pertenece x xs
                       then  xs
                       else (x:xs))


belongs x (Set []) = False
belongs x (Set (y:ys)) = x == y || belongs x (Set ys)

sizeS (Set xs) = longitud  xs

removeS x (Set (y:ys)) = if x == y
                         then Set ys
                         else removeS x (Set (agregarAlFinal ys y))

unionS (Set xs) (Set ys) = Set (xs ++ ys) 


setToList (Set xs) = xs
{-setToList (Set (x:xs)) = if longitud (x:xs) == 0
                         then []
                         else if x belongs (Set xs)
                              then setToList (Set xs)
                              else x : setToList (Set xs) -}

agregar :: [a] -> [a] -> [a]
agregar [] b = b
agregar (x:xs) b = x : agregar xs b

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

agregarAlFinal :: [a] -> a -> [a]
--agregarAlFinal [] b = b : []
agregarAlFinal a b = agregar a [b] 


pertenece :: Eq a => a -> [a] -> Bool
pertenece  e [] = False
pertenece e (x:xs) = e == x  || pertenece e xs

{- data Stock a = Sk [a]
   
   data Queue = Q [a] [a] -------doble lista
   
   dequeue :: Queue a -> Queue a
   dequeue (Q xs ys) =
                 let(xs',ys') =  devolverListasModificadas xs ys
                 in Q xs' ys' -}