module SetV2
(Set,emptyS,addS,belongs,sizeS,removeS, unionS, setToList) where

    data Set a = Set [a] Int 

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]

emptyS = Set [] 0
-------------------------------

addS e (Set [] x) = (Set [e] x + 1)
addS e (Set xs y) = if pertenece e xs then (Set xs y) else (Set (e : xs) y + 1) -- cuadratica

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys


-------------------------------------------------------------------------------------

belongs e (Set [] _) = False 
belongs e  (Set (x:xs) y) = e == x || belongs e (Set xs y)

---------------------------------------------------------------------------------------

sizeS (Set _ x) = x

------------------------------------------------------------------------------------

removeS e (Set [] x) = 
removeS e (Set (x:xs) y) = if e == x then (Set xs (y - 1)) else removeS e (Set (agregarAlFinal xs x) y) 


agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = if estaVacia xs then x : y : xs else x : agregarAlFinal xs y  


estaVacia :: [a] -> Bool    
estaVacia(x:xs) = False 
estaVacia _ = True

------------------------------------------------------------------------------------------

unionS (Set xs n1) (Set ys n2) = (Set xs ++ ys (n1 + n2))

------------------------------------------------------------------------------------------
setToList (Set [] _) = []
setToList (Set (x:xs) y) = x : setToList (Set xs y)

