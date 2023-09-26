module Queue
(Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue)
where

data Queue a = Q [a] deriving Show 

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

emptyQ = Q []

-----------------------------------

isEmptyQ (Q xs) = null xs

------------------------------------------
enqueue x (Q ys) = Q (agregarAlFinal ys x)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = if estaVacia xs then x : y : xs else x : agregarAlFinal xs y  


estaVacia :: [a] -> Bool    
estaVacia(x:xs) = False 
estaVacia _ = True

---------------------------------------------------------------------------------------------

--firstQ (Q (x:xs)) = if null xs then x else firstQ (Q xs)
firstQ (Q xs) = head xs 

--------------------------------------------------------

dequeue (Q xs) = Q (tail xs)

--------------------------------------------------------
