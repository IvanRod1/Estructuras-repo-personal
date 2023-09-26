--import SetV1
import Queue
import Stack

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

{-set1 = addS 10 (addS 20 (addS 30 emptyS))
set2 = addS 40 (addS 50 (addS 60 emptyS))
set3 = addS 70 (addS 80 (addS 90 emptyS))-}

queue1 = enqueue 2 (enqueue 1 emptyQ)
queue2 = enqueue 4 (enqueue 3 emptyQ)

{-arbol1 :: Tree (Set Integer)
arbol1 = NodeT set1 (NodeT set2 EmptyT EmptyT) (NodeT set3 EmptyT EmptyT)

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) st = if belongs x st then x : losQuePertenecen xs st else losQuePertenecen xs st-}

------------------------------------------------------------------------------------------------------------
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =  if pertenece x xs then  sinRepetidos xs else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece  e [] = False
pertenece e (x:xs) = e == x  || pertenece e xs

------------------------------------------------------------

{-unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s ts1 ts2) = unionS (unionS s (unirTodos ts1)) (unirTodos ts2)-}

---------------------------------------------------------------------------------

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q then 0 else 1 + lengthQ (dequeue q)

---------------------------------------------------------------------------------

queueToList :: Queue a -> [a]

queueToList q = if isEmptyQ q then [] else agregarAlFinal (queueToList (dequeue q)) (firstQ q)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = if estaVacia xs then x : y : xs else x : agregarAlFinal xs y  


estaVacia :: [a] -> Bool    
estaVacia(x:xs) = False 
estaVacia _ = True

-------------------------------------------------------------------------------------

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = agregarListAQueue (queueToList q2) q1

agregarListAQueue :: [a] -> Queue a -> Queue a
agregarListAQueue [] q = q
agregarListAQueue (x:xs) q = enqueue x (agregarListAQueue xs q)

-----------------------------------------------------------------------------------
stack1 = push 20 (push 10 emptyS)

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)    

-- [1,2,3]  Q[1,2,3]

----------------------------------------------------------------------------------------

desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s then [] else top s : desapilar (pop s)

-----------------------------------------------------------------------------------------

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos n x s = apilar(agregarEnPos_ n x (desapilar s)) 

agregarEnPos_ :: Int -> a -> [a] -> [a]
agregarEnPos_ _ x [] = [x]
agregarEnPos_ 1 x ys = (x:ys)
agregarEnPos_ n x (y:ys) = y : agregarEnPos_ (n - 1) x ys

------------------------------------------------------------------------------------------------
