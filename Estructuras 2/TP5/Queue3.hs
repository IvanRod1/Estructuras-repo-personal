module Queue3
(Queue,emptyQ,isEmptyQ,enqueue,dequeue)
where

data Queue a = Q [a] [a] deriving Show
-- Queue fs bs
{-Quitaremos elementos a través de fs y agregaremos a través de bs, pero
todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
vacía, entonces la cola se encuentra vacía.-}

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
--firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

queue1 = Q [1,2,3] [10,20,30]

emptyQ = Q [] []
------------------------------

isEmptyQ (Q fs _) = null fs 

----------------------------------

enqueue x (Q fs bs) = (Q fs (agregarAlFinal bs x))

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = if estaVacia xs then x : y : xs else x : agregarAlFinal xs y  


estaVacia :: [a] -> Bool    
estaVacia(x:xs) = False 
estaVacia _ = True

--------------------------------------------------------------------------------------------

dequeue (Q fs bs) = if null fs then emptyQ else Q (tail fs) bs